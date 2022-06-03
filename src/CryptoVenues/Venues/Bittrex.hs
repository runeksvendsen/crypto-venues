{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-
   https://bittrex.github.io/api/v3
-}
module CryptoVenues.Venues.Bittrex
()
where

import CryptoVenues.Internal.CPrelude
import CryptoVenues.Venues.Common.ScientificOrder (QuotedScientific)
import OrderBook
import CryptoVenues.Fetch
import CryptoVenues.Types.Market
import CryptoVenues.Types.MarketSymbol

import qualified Servant.Client        as SC
import Servant.API
import qualified Data.Aeson   as Json
import qualified Data.Aeson.Types   as Json
import           Data.Vector  (Vector)
import qualified Data.Vector as Vec

instance Json.FromJSON (SomeBook "bittrex") where
   parseJSON val =
      let fromBook Book{..} = mkSomeBook
            <$> traverse parseOrder (fromMaybe Vec.empty bid)
            <*> traverse parseOrder (fromMaybe Vec.empty ask)
      in Json.parseJSON val >>= fromBook >>= either fail return

data Book = Book
   { bid :: Maybe (Vector BittrexOrder)
   , ask :: Maybe (Vector BittrexOrder)
   } deriving (Show, Generic)

instance Json.FromJSON Book

data BittrexOrder = BittrexOrder
   { quantity  :: QuotedScientific
   , rate      :: QuotedScientific
   } deriving (Show, Generic)

instance Json.FromJSON BittrexOrder where
  parseJSON  = Json.genericParseJSON
      Json.defaultOptions

parseOrder :: BittrexOrder -> Json.Parser SomeOrder
parseOrder bo@BittrexOrder{..} =
   maybe (fail $ "Bad BittrexOrder: " ++ show bo) return soM
   where soM = mkSomeOrder (toRational quantity) (toRational rate)

apiUrl :: BaseUrl
apiUrl = BaseUrl Https "api.bittrex.com" 443 ""

-- | Example: https://api.bittrex.com/v3/markets/BTC-USD/orderbook
type Api base quote
   = "v3"
   :> "markets"
   :> Capture "symbol" (MarketSymbol "bittrex")
   :> "orderbook"
   :> QueryParam "depth" Depth
   :> Get '[JSON] (SomeBook "bittrex")

mkBookSrc :: MarketSymbol "bittrex" -> DataSrc (SomeBook "bittrex")
mkBookSrc pair = DataSrc apiUrl (clientM pair (Just MaxDepth))
   where
   clientM = SC.client (Proxy :: Proxy (Api base quote))

data BMarket = BMarket
   { baseCurrencySymbol :: Text
   , quoteCurrencySymbol :: Text
   , symbol :: Text
   , status :: Status
   } deriving (Show, Generic)

data Depth
   = MaxDepth
   | DefaultDepth
   | MinDepth

depthInteger :: Depth -> Int
depthInteger = \case
   MaxDepth -> 500
   DefaultDepth -> 25
   MinDepth -> 1

instance ToHttpApiData Depth where
  toUrlPiece = toS . show . depthInteger
  toQueryParam = toS . show . depthInteger

data Status
   = Online
   | Unknown
       deriving (Eq, Show)

statusFromText :: Text -> Status
statusFromText = \case
   "ONLINE" -> Online
   _ -> Unknown

instance Json.FromJSON Status where
   parseJSON = Json.withText "Status" (pure . statusFromText)

instance Json.FromJSON BMarket where
  parseJSON = Json.genericParseJSON
      Json.defaultOptions

-- | https://api.bittrex.com/v3/markets
type ApiMarkets
   = "v3"
   :> "markets"
   :> Get '[JSON] (MarketList "bittrex")

instance MarketBook "bittrex" where
   marketBook = mkBookSrc
   rateLimit = DataSrc apiUrl (return 1)
   -- Rate limit: 1 per second (https://bitcoin.stackexchange.com/questions/59316/trading-bot-what-is-the-maximum-load-an-exchange-server-can-take)

instance EnumMarkets "bittrex" where
   allMarkets = DataSrc apiUrl clientM
      where
         clientM = SC.client (Proxy :: Proxy ApiMarkets)

instance Json.FromJSON (MarketList "bittrex") where
   parseJSON val = do
      result <- Json.parseJSON val
      return $ MarketList $ map fromBM $ filter ((== Online) . status) result

fromBM :: BMarket -> Market venue
fromBM BMarket{..} =
   Market
      { miBase       = baseCurrencySymbol
      , miQuote      = quoteCurrencySymbol
      , miApiSymbol  = toMarketSymbol symbol
      }

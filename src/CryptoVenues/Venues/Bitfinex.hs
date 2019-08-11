{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module CryptoVenues.Venues.Bitfinex
()
where

import CryptoVenues.Internal.CPrelude     hiding (asks)
import OrderBook
import CryptoVenues.Fetch
import CryptoVenues.Types.Market
import CryptoVenues.Venues.Common.ScientificOrder

import qualified Servant.Client        as SC
import Servant.API
import qualified Data.Aeson   as Json
import qualified Data.Aeson.Types   as Json
import           Data.Vector  (Vector)
import qualified Data.Vector  as Vec
import qualified Data.Text as T


instance EnumMarkets "bitfinex" where
   allMarkets = DataSrc apiUrl clientM
      where
         clientM = SC.client (Proxy :: Proxy ApiMarkets)

instance MarketBook "bitfinex" where
   marketBook apiSymbol = DataSrc apiUrl cm
      where cm = clientM apiSymbol (Just 1000) (Just 1000)
            clientM = SC.client (Proxy :: Proxy (Api base quote))
   rateLimit = DataSrc apiUrl (return 1)
-- Orderbook rate limit: Ratelimit: 60 req/min (https://docs.bitfinex.com/v1/reference#rest-public-orderbook)

instance Json.FromJSON (SomeBook "bitfinex") where
   parseJSON val =
      let fromBook Book{..} = mkSomeBook
                                    -- HACK: ignore zero-price buy orders from Bitfinex
            <$> traverse parseOrder (Vec.filter nonZeroPrice bids)
            <*> traverse parseOrder asks
      in Json.parseJSON val >>= fromBook >>= either fail return

data Book = Book
   { bids :: Vector BitfinexOrder
   , asks :: Vector BitfinexOrder
   } deriving (Eq, Show, Generic)

data BitfinexOrder = BitfinexOrder
   { price     :: QuotedScientific
   , amount    :: QuotedScientific
   , timestamp :: String
   } deriving (Eq, Show, Generic)

nonZeroPrice :: BitfinexOrder -> Bool
nonZeroPrice BitfinexOrder{..} = price /= 0

instance Json.FromJSON Book
instance Json.FromJSON BitfinexOrder

parseOrder :: BitfinexOrder -> Json.Parser SomeOrder
parseOrder BitfinexOrder{..} = either fail return $ parseSomeOrderSci price amount

apiUrl :: BaseUrl
apiUrl = BaseUrl Https "api.bitfinex.com" 443 ""

type Api base quote
   = "v1"
   :> "book"
   :> Capture "symbol" Text
   :> QueryParam "limit_bids" Word
   :> QueryParam "limit_asks" Word
   :> Get '[JSON] (SomeBook "bitfinex")

-- | https://api.bitfinex.com/v1/symbols
type ApiMarkets
   = "v1"
   :> "symbols"
   :> Get '[JSON] (MarketList "bitfinex")

instance Json.FromJSON (MarketList "bitfinex") where
   parseJSON val = MarketList <$> Json.parseJSON val

-- | Bitfinex "(trading) pair code" string format (deduced through trial and error):
-- Either (in descending priority):
--  a) exactly six charaters in length:
--       * first three characters comprise base currency
--       * following three characters comprise quote currency
--  b) containing exactly one colon (':') character:
--       * base/quote currency separated by colon (':') character
fromPairCode
   :: T.Text
   -> Maybe (Market "bitfinex")
fromPairCode str
   | T.length str == 6 =
      mkMarket (T.toUpper $ T.take 3 str) (T.toUpper $ T.takeEnd 3 str)
   | [baseSymbol, quoteSymbol] <- T.split (== ':') str =
      mkMarket baseSymbol quoteSymbol
   | otherwise = Nothing
  where
   mkMarket base quote =
      Just $ Market
            { miBase       = base
            , miQuote      = quote
            , miApiSymbol  = str
            }

instance Json.FromJSON (Market "bitfinex") where
   parseJSON = Json.withText "Bitfinex market" $ \currPair ->
         let errorStr = "Invalid trading pair code: " ++ toS currPair
         in maybe (fail errorStr) return (fromPairCode currPair)

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module CryptoVenues.Venues.Bitstamp
()
where

import CryptoVenues.Internal.CPrelude
import OrderBook
import CryptoVenues.Fetch
import CryptoVenues.Types.Market
import CryptoVenues.Types.MarketSymbol
import CryptoVenues.Venues.Common.ScientificOrder

import qualified Servant.Client        as SC
import Servant.API
import Data.Aeson (FromJSON(parseJSON), (.:), (.:?))
import qualified Data.Aeson   as Json
import qualified Data.Aeson.Types   as Json
import qualified Data.Text as T
import           Data.Vector  (Vector)
import qualified Network.HTTP.Types.Status   as Status
import qualified Data.ByteString.Lazy.UTF8   as BS
import Data.List                             (isInfixOf)
import qualified CryptoVenues.Internal.Log   as Log
import qualified Data.Vector  as Vec
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}


instance MarketBook "bitstamp" where
   marketBook apiSymbol = DataSrc apiUrl (cm apiSymbol (Just userAgent))
      where cm = SC.client (Proxy :: Proxy ApiOb)
   rateLimit = DataSrc apiUrl (return 1)
   -- Rate limit: 600 requests per 10 minutes (https://www.bitstamp.net/api/)

instance EnumMarkets "bitstamp" where
   allMarkets = DataSrc apiUrl clientM
      where
         clientM = SC.client (Proxy :: Proxy ApiMarkets)
   apiQuirk _ se@(SC.FailureResponse req res)
      | statusCode == 400 = SC.FailureResponse req <$> handleIncapsulaErr
      | otherwise = return se
    where
      handleIncapsulaErr =
            let bodyStr = BS.toString (SC.responseBody res)
            in if "iframe" `isInfixOf` bodyStr && "Incapsula_Resource" `isInfixOf` bodyStr
                  then do
                      Log.infoS "bitstamp" ("Applying quirk. Response:\n" <> toS (show res))
                      return $ res { SC.responseStatusCode = Status.status429 }
                  else return res
      statusCode = Status.statusCode (SC.responseStatusCode res)
   apiQuirk _ se = return se

apiUrl :: BaseUrl
apiUrl = BaseUrl Https "www.bitstamp.net" 443 ""

userAgent :: Text
userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/66.0.3359.181 Safari/537.36"


-- Orderbook
type ApiOb -- Example: https://www.bitstamp.net/api/v2/order_book/btcpax
   = "api"
   :> "v2"
   :> "order_book"
   :> Capture "symbol" (MarketSymbol "bitstamp")
   :> Header "User-Agent" Text
   :> Get '[JSON] (SomeBook "bitstamp")

data Book = Book
   { timestamp :: Text
   , bids :: Vector BitstampOrder
   , asks :: Vector BitstampOrder
   } deriving (Eq, Show, Generic)

instance Json.FromJSON Book where
   parseJSON = Json.withObject "Bistamp orderbook" $ \o -> do
      mBids <- o .:? "bids"
      mAsks <- o .:? "asks"
      timestamp <- o .: "timestamp"
      pure $ Book
         { timestamp = timestamp
         , bids = fromMaybe mempty mBids
         , asks = fromMaybe mempty mAsks
         }

type BitstampOrder = (QuotedScientific,QuotedScientific)   -- Price, Quantity

parseOrder :: BitstampOrder -> Json.Parser SomeOrder
parseOrder (price,qty) = either fail return $ parseSomeOrderSci price qty

nonZeroPrice :: BitstampOrder -> Bool
nonZeroPrice (price, _) = price /= 0

instance Json.FromJSON (SomeBook "bitstamp") where
   parseJSON val =
      let fromBook Book{..} = mkSomeBook
            <$> traverse parseOrder (Vec.filter nonZeroPrice bids)
            <*> traverse parseOrder (Vec.filter nonZeroPrice asks)
      in Json.parseJSON val >>= fromBook >>= either fail return


-- Markets
type ApiMarkets -- https://www.bitstamp.net/api/v2/trading-pairs-info/
   = "api"
   :> "v2"
   :> "trading-pairs-info"
   :> Get '[JSON] (MarketList "bitstamp")

data BMarket = BMarket
   { base_decimals      :: Word
   , name               :: Text -- E.g. "LTC/USD"
   , counter_decimals   :: Word
   , trading            :: TradingStatus
   , url_symbol         :: Text -- E.g. "ltcusd"
   } deriving (Eq, Show, Generic)

data TradingStatus
   = Enabled
   | Disabled
   | UnknownTradingStatus
      deriving (Eq, Show, Generic)

instance Json.FromJSON TradingStatus where
   parseJSON =
      let fromText "Enabled" = Enabled
          fromText "Disabled" = Disabled
          fromText _ = UnknownTradingStatus
      in Json.withText "trading status" (pure . fromText)

instance Json.FromJSON BMarket

instance Json.FromJSON (MarketList "bitstamp") where
   parseJSON val = do
      bMarketLst <- filter ((== Enabled) . trading) <$> Json.listParser Json.parseJSON val
      MarketList <$> traverse fromBMarket bMarketLst

instance Json.FromJSON (Market "bitstamp") where
   parseJSON val = Json.parseJSON val >>= fromBMarket

fromBMarket :: BMarket -> Json.Parser (Market venue)
fromBMarket BMarket{..} =
   case T.split (== '/') name of
      [base,quote] -> return Market
            { miBase       = T.toUpper base
            , miQuote      = T.toUpper quote
            , miApiSymbol  = toMarketSymbol url_symbol
            }
      _            -> fail . toS $ "Bad market name: " <> name

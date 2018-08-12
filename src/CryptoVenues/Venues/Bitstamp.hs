{-# OPTIONS_GHC -fno-warn-orphans #-}
module CryptoVenues.Venues.Bitstamp
()
where

import CryptoVenues.Internal.CPrelude
import OrderBook
import CryptoVenues.Fetch
import CryptoVenues.Types.Market
import CryptoVenues.Venues.Common.StringArrayOrder  (parseSomeOrderStr)

import qualified Servant.Client        as SC
import Servant.API
import qualified Data.Aeson   as Json
import qualified Data.Aeson.Types   as Json
import qualified Data.Text as T
import           Data.Vector  (Vector)
import Control.Monad.Fail
import qualified Network.HTTP.Types.Status   as Status
import qualified Data.ByteString.Lazy.UTF8   as BS
import Data.List                             (isInfixOf)
import qualified CryptoVenues.Internal.Log   as Log
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}


instance MarketBook "bitstamp" where
   marketBook apiSymbol = DataSrc apiUrl (cm apiSymbol (Just userAgent))
      where cm = SC.client (Proxy :: Proxy ApiOb)
   rateLimit = DataSrc apiUrl (return . fromRational . toRational $ 1)
   -- Rate limit: 600 requests per 10 minutes (https://www.bitstamp.net/api/)

instance EnumMarkets "bitstamp" where
   allMarkets = DataSrc apiUrl clientM
      where
         clientM = SC.client (Proxy :: Proxy ApiMarkets)
   apiQuirk _ se@(SC.FailureResponse res)
      | statusCode == 400 = SC.FailureResponse <$> handleIncapsulaErr
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

-- Base URL
apiUrl = BaseUrl Https "www.bitstamp.net" 443 ""

userAgent :: Text
userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/66.0.3359.181 Safari/537.36"


-- Orderbook
type ApiOb
   = "api"
   :> "v2"
   :> "order_book"
   :> Capture "symbol" Text
   :> Header "User-Agent" Text
   :> Get '[JSON] (SomeBook "bitstamp")

data Book = Book
   { timestamp :: Text
   , bids :: Vector BitstampOrder
   , asks :: Vector BitstampOrder
   } deriving (Eq, Show, Generic)
instance Json.FromJSON Book

type BitstampOrder = (String,String)   -- Price, Quantity

parseOrder :: BitstampOrder -> Json.Parser SomeOrder
parseOrder (price,qty) = either fail return $ parseSomeOrderStr price qty

instance Json.FromJSON (SomeBook "bitstamp") where
   parseJSON val =
      let fromBook Book{..} = mkSomeBook
            <$> traverse parseOrder bids
            <*> traverse parseOrder asks
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
   , trading            :: Text -- E.g. "Enabled"
   , url_symbol         :: Text -- E.g. "ltcusd"
   } deriving (Eq, Show, Generic)

instance Json.FromJSON BMarket

instance Json.FromJSON (MarketList "bitstamp") where
   parseJSON val = MarketList <$> Json.parseJSON val

instance Json.FromJSON (Market "bitstamp") where
   parseJSON val = Json.parseJSON val >>= fromBMarket
      where
      fromBMarket BMarket{..} =
         case T.split (== '/') name of
            [base,quote] -> return Market
                  { miBase       = T.toUpper base
                  , miQuote      = T.toUpper quote
                  , miApiSymbol  = url_symbol
                  }
            _            -> fail . toS $ "Bad market name: " <> name




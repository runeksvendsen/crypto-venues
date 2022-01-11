{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module CryptoVenues.Venues.Coinbase
()
where

import CryptoVenues.Internal.CPrelude
import OrderBook
import CryptoVenues.Fetch
import CryptoVenues.Types.Market
import CryptoVenues.Types.MarketSymbol
import CryptoVenues.Venues.Common.ScientificOrder

import qualified Servant.Client                       as SC
import           Servant.API
import           Data.Vector                          (Vector)
import qualified Data.Aeson.Types                     as Json


instance EnumMarkets "coinbase" where
   allMarkets = DataSrc endpoint_url $ clientM (Just userAgent)
      where
         clientM = SC.client (Proxy :: Proxy ApiMarkets)

instance MarketBook "coinbase" where
   marketBook apiSymbol = DataSrc endpoint_url cm
      where cm = clientM apiSymbol (Just 3) (Just userAgent)
            clientM = SC.client (Proxy :: Proxy ApiOb)
   rateLimit = DataSrc endpoint_url (return 3)
-- Orderbook rate limit: 3 req/sec (https://docs.pro.coinbase.com/#rate-limits)

endpoint_url :: SC.BaseUrl
endpoint_url = BaseUrl Https "api.pro.coinbase.com" 443 ""

type ApiMarkets
   = "products"
   :> Header "User-Agent" Text
   :> Get '[JSON] (MarketList "coinbase")

type ApiOb
   = "products"
   :> Capture "symbol" (MarketSymbol "coinbase")
   :> "book"
   :> QueryParam "level" Word
   :> Header "User-Agent" Text
   :> Get '[JSON] (SomeBook "coinbase")


data CMarket = CMarket
   { id              :: Text  -- E.g. "BTC-USD"
   , base_currency   :: Text  -- E.g. "BTC"
   , quote_currency  :: Text  -- E.g. "USD"
   , status          :: Text
   } deriving (Eq, Show, Generic)

instance Json.FromJSON CMarket

instance Json.FromJSON (MarketList "coinbase") where
   parseJSON val = MarketList . map fromCMarket . filter isNotDelisted <$> Json.parseJSON val
      where
      isNotDelisted cMarket = status cMarket /= "delisted"
      fromCMarket CMarket{..} =
         Market
            { miBase       = base_currency
            , miQuote      = quote_currency
            , miApiSymbol  = toMarketSymbol id
            }

data Book = Book
   { sequence  :: Word
   , bids      :: Vector CoinbaseOrder
   , asks      :: Vector CoinbaseOrder
   } deriving (Eq, Show, Generic)

instance Json.FromJSON Book

type CoinbaseOrder = (QuotedScientific,QuotedScientific,Text)   -- Price, Quantity, Order_Count

parseOrder :: CoinbaseOrder -> Json.Parser SomeOrder
parseOrder (price,qty,_) = either fail return $ parseSomeOrderSci price qty

instance Json.FromJSON (SomeBook "coinbase") where
   parseJSON val =
      let fromBook Book{..} = mkSomeBook
            <$> traverse parseOrder bids
            <*> traverse parseOrder asks
      in Json.parseJSON val >>= fromBook >>= either fail return

-- TODO: Use custom user agent
userAgent :: Text
userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.84 Safari/537.36"

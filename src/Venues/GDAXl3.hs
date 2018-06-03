module Venues.GDAXl3
()
where

import CPrelude
import OrderBook
import Fetch
import Types.Market
import Venues.Common.StringArrayOrder  (parseOrderStr)

import qualified Servant.Client as SC
import Servant.API
import qualified Data.Aeson   as Json
import           Data.Vector  (Vector)
--import Control.Monad.Fail
import qualified Data.Aeson.Types   as Json


instance Json.FromJSON (OrderBook "gdax-l3" base quote) where
   parseJSON val =
      let fromBook Book{..} = OrderBook
            <$> traverse parseOrder bids
            <*> traverse parseOrder asks
      in Json.parseJSON val >>= fromBook

data Book = Book
   { sequence :: Word            -- https://docs.gdax.com/#sequence-numbers
   , bids :: Vector GDAXl3Order
   , asks :: Vector GDAXl3Order
   } deriving (Eq, Show, Generic)

instance Json.FromJSON Book

type GDAXl3Order = (String,String,String)   -- Price, Quantity, Order_Id

parseOrder :: GDAXl3Order -> Json.Parser (Order base quote)
parseOrder (price,qty,_) = either fail return $ parseOrderStr price qty

gdax :: SC.BaseUrl
gdax = BaseUrl Https "api.gdax.com" 443 ""

type Api base quote
   = "products"
   :> Capture "symbol" Text
   :> "book"
   :> QueryParam "level" Word
   :> Header "User-Agent" Text
   :> Get '[JSON] (OrderBook "gdax-l3" base quote)

--instance DataSource (OrderBook "gdax-l3" "BTC" "USD") where
--   dataSrc = DataSrc gdax (clientM "BTC-USD" (Just 3) (Just userAgent))
--      where
--         clientM = SC.client (Proxy :: Proxy (Api "BTC" "USD"))

--instance DataSource (OrderBook "gdax-l3" "BTC" "EUR") where
--   dataSrc = DataSrc gdax (clientM "BTC-EUR" (Just 3) (Just userAgent))
--      where
--         clientM = SC.client (Proxy :: Proxy (Api "BTC" "EUR"))

-- TODO: Use custom user agent
userAgent :: Text
userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.84 Safari/537.36"

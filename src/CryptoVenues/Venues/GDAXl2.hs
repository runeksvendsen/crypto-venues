{-# OPTIONS_GHC -fno-warn-orphans #-}
module CryptoVenues.Venues.GDAXl2

()
where

import CryptoVenues.Internal.CPrelude
import OrderBook
import CryptoVenues.Fetch
import CryptoVenues.Types.Market
import CryptoVenues.Venues.Common.StringArrayOrder  (parseOrderStr)

import qualified Servant.Client        as SC
import Servant.API
import qualified Data.Aeson   as Json
import           Data.Vector  (Vector)
import Control.Monad.Fail
import qualified Data.Aeson.Types   as Json

instance Json.FromJSON (OrderBook "gdax-l2" base quote) where
   parseJSON val =
      let fromBook Book{..} = OrderBook
            <$> (BuySide <$> traverse parseOrder bids)
            <*> (SellSide <$> traverse parseOrder asks)
      in Json.parseJSON val >>= fromBook

data Book = Book
   { sequence :: Word            -- https://docs.gdax.com/#sequence-numbers
   , bids :: Vector GDAXl2Order
   , asks :: Vector GDAXl2Order
   } deriving (Eq, Show, Generic)

instance Json.FromJSON Book

type GDAXl2Order = (String,String,Word)   -- Price, Quantity, Order_Count

parseOrder :: GDAXl2Order -> Json.Parser (Order base quote)
parseOrder (price,qty,_) = either fail return $ parseOrderStr price qty

gdax :: SC.BaseUrl
gdax = BaseUrl Https "api.gdax.com" 443 ""

type Api base quote
   = "products"
   :> Capture "symbol" Text
   :> "book"
   :> QueryParam "level" Word
   :> Header "User-Agent" Text
   :> Get '[JSON] (OrderBook "gdax-l2" base quote)

--instance DataSource (OrderBook "gdax-l2" "BTC" "USD") where
--   dataSrc = DataSrc gdax (clientM "BTC-USD" (Just 2) (Just userAgent))
--      where
--         clientM = SC.client (Proxy :: Proxy (Api "BTC" "USD"))

--instance DataSource (OrderBook "gdax-l2" "BTC" "EUR") where
--   dataSrc = DataSrc gdax (clientM "BTC-EUR" (Just 2) (Just userAgent))
--      where
--         clientM = SC.client (Proxy :: Proxy (Api "BTC" "EUR"))

-- TODO

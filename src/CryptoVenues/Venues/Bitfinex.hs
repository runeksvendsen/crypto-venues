{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CryptoVenues.Venues.Bitfinex
()
where

import CryptoVenues.Internal.CPrelude     hiding (asks)
import OrderBook
import CryptoVenues.Fetch
import CryptoVenues.Types.Market
import CryptoVenues.Venues.Common.StringArrayOrder  (parseSomeOrderStr)

import qualified Servant.Client        as SC
import Servant.API
import qualified Data.Aeson   as Json
import qualified Data.Aeson.Types   as Json
--import qualified Data.Aeson.Parser as Json
import           Data.Vector  (Vector)
import Control.Monad.Fail
import qualified Data.Text as T


instance EnumMarkets "bitfinex" where
   allMarkets = DataSrc apiUrl clientM
      where
         clientM = SC.client (Proxy :: Proxy ApiMarkets)

instance MarketBook "bitfinex" where
   marketBook apiSymbol = DataSrc apiUrl cm
      where cm = clientM apiSymbol (Just 1000) (Just 1000)
            clientM = SC.client (Proxy :: Proxy (Api base quote))
   rateLimit = DataSrc apiUrl (return . fromRational . toRational $ 1)
-- Orderbook rate limit: Ratelimit: 60 req/min (https://docs.bitfinex.com/v1/reference#rest-public-orderbook)

instance Json.FromJSON (SomeBook "bitfinex") where
   parseJSON val =
      let fromBook Book{..} = mkSomeBook
            <$> traverse parseOrder bids
            <*> traverse parseOrder asks
      in Json.parseJSON val >>= fromBook >>= either fail return

data Book = Book
   { bids :: Vector BitfinexOrder
   , asks :: Vector BitfinexOrder
   } deriving (Eq, Show, Generic)

data BitfinexOrder = BitfinexOrder
   { price     :: String
   , amount    :: String
   , timestamp :: String
   } deriving (Eq, Show, Generic)

instance Json.FromJSON Book
instance Json.FromJSON BitfinexOrder

parseOrder :: BitfinexOrder -> Json.Parser SomeOrder
parseOrder BitfinexOrder{..} = either fail return $ parseSomeOrderStr price amount

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

instance Json.FromJSON (Market "bitfinex") where
   parseJSON = Json.withText "Bitfinex market" $ \currPair ->
         if T.length currPair /= 6
            then fail $ "Invalid symbol: " ++ toS currPair
            else return Market
                  { miBase       = T.toUpper $ T.take 3 currPair
                  , miQuote      = T.toUpper $ T.takeEnd 3 currPair
                  , miApiSymbol  = currPair
                  }


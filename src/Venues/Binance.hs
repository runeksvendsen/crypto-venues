module Venues.Binance where

import CPrelude
import Markets
import OrderBook
import Fetch
import Venues.Common.StringArrayOrder  (parseSomeOrderStr)
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import Servant.API
import qualified Data.Aeson   as Json
import qualified Data.Aeson.Types   as Json
import           Data.Vector  (Vector)
import Control.Monad.Fail
import qualified Data.Text as T
import qualified Data.Vector  as Vec


apiUrl :: S.BaseUrl
apiUrl = S.BaseUrl S.Https "api.binance.com" 443 "/api/v1"

-- | https://api.binance.com/api/v1/exchangeInfo
type ApiMarkets
   =  "exchangeInfo"
   :> Get '[JSON] (MarketList "binance")

instance DataSource (MarketList "binance") where
   dataSrc = DataSrc apiUrl clientM
      where
         clientM = SC.client (Proxy :: Proxy ApiMarkets)

instance Json.FromJSON (MarketList "binance") where
   parseJSON val =
         let conv = map fromSymbol . filter ((== "TRADING") . status) . symbols
         in  MarketList . conv <$> Json.parseJSON val

--instance Json.FromJSON (Market "binance") where
--   parseJSON val = fromSymbol <$> Json.parseJSON val


newtype ExchangeInfo = ExchangeInfo
   { symbols :: [BSymbol]
   } deriving Generic

instance Json.FromJSON ExchangeInfo

data BSymbol = BSymbol
    { symbol               :: Text  -- e.g. "ETHBTC"
    , status               :: Text  -- =? "TRADING"
    , baseAsset            :: Text
    , baseAssetPrecision   :: Word
    , quoteAsset           :: Text
    , quotePrecision       :: Word
    } deriving Generic

instance Json.FromJSON BSymbol

fromSymbol :: BSymbol -> Market "binance"
fromSymbol BSymbol{..} = Market baseAsset quoteAsset symbol


type ApiDepth
   =  "depth"
   :> QueryParam "symbol" Text
   :> QueryParam "limit" Word    -- Legal values: '5, 10, 20, 50, 100, 500, 1000'
   :> Get '[JSON] (SomeBook "binance")

data Book = Book
   { bids   :: Vector (Text,Text,[Text])
   , asks   :: Vector (Text,Text,[Text])
   } deriving Generic

instance Json.FromJSON Book

instance MarketBook "binance" where
   marketBook apiSymbol = DataSrc apiUrl cm
      where cm = clientM (Just apiSymbol) (Just 1000)
            clientM = SC.client (Proxy :: Proxy ApiDepth)


instance Json.FromJSON (SomeBook "binance") where
   parseJSON val =
      let fromBook Book{..} = mkSomeBook
            <$> traverse parseOrder bids
            <*> traverse parseOrder asks
      in Json.parseJSON val >>= fromBook >>= either fail return


parseOrder :: (Text,Text,[Text]) -> Json.Parser SomeOrder
parseOrder (price,amount,_) = parseSomeOrderStr (toS price) (toS amount)


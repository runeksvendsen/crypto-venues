{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module CryptoVenues.Venues.Binance where

import CryptoVenues.Internal.CPrelude
import OrderBook
import CryptoVenues.Fetch
import CryptoVenues.Types.Market
import CryptoVenues.Venues.Common.ScientificOrder

import qualified Servant.Client        as SC
import Servant.API
import qualified Data.Aeson   as Json
import qualified Data.Aeson.Types   as Json
import           Data.Vector  (Vector)


apiUrl :: BaseUrl
apiUrl = BaseUrl Https "api.binance.com" 443 "/api/v1"

-- | https://api.binance.com/api/v1/exchangeInfo
type ApiMarkets
   =  "exchangeInfo"
   :> Get '[JSON] (MarketList "binance")

instance EnumMarkets "binance" where
   allMarkets = DataSrc apiUrl clientM
      where
         clientM = SC.client (Proxy :: Proxy ApiMarkets)

instance Json.FromJSON (MarketList "binance") where
   parseJSON val =
         let conv = map fromSymbol . filter ((== "TRADING") . status) . symbols
         in  MarketList . conv <$> Json.parseJSON val

data ExchangeInfo = ExchangeInfo
   { symbols      :: [BSymbol]
   , rateLimits   :: [BRateLimit]
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
   { bids   :: Vector BOrder
   , asks   :: Vector BOrder
   } deriving Generic

type BOrder = (QuotedScientific,QuotedScientific)

instance Json.FromJSON Book

instance MarketBook "binance" where
   marketBook apiSymbol = DataSrc apiUrl cm
      where cm = clientM (Just apiSymbol) (Just 1000)
            clientM = SC.client (Proxy :: Proxy ApiDepth)
   rateLimit = DataSrc apiUrl clientM
      where
         clientM = SC.client (Proxy :: Proxy ApiRateLimit)

instance Json.FromJSON (RateLimit "binance") where
   parseJSON val =
      let fromEI ExchangeInfo{..} =
            headMay $ filter ((==) REQUEST_WEIGHT . rateLimitType) rateLimits
      in Json.parseJSON val >>=
            maybe (fail "rateLimitType 'REQUEST_WEIGHT' not found") (return . perSecond) . fromEI


instance Json.FromJSON (SomeBook "binance") where
   parseJSON val =
      let fromBook Book{..} = mkSomeBook
            <$> traverse parseOrder bids
            <*> traverse parseOrder asks
      in Json.parseJSON val >>= fromBook >>= either fail return

parseOrder :: BOrder -> Json.Parser SomeOrder
parseOrder (price,amount) = either fail return $ parseSomeOrderSci price amount

-- Rate limit stuff
type ApiRateLimit
   =  "exchangeInfo"
   :> Get '[JSON] (RateLimit "binance")

-- Rate limit: https://github.com/binance-exchange/binance-official-api-docs/blob/master/rest-api.md#order-book
data BRateLimit = BRateLimit
   { rateLimitType  :: LimitType
   , interval       :: LimitInterval
   , limit          :: Word
   } deriving Generic

perSecond :: BRateLimit -> RateLimit "binance"
perSecond (BRateLimit _ SECOND limit) = fromRational $ toRational limit
perSecond (BRateLimit _ MINUTE limit) = fromRational (fromIntegral limit % 60)
perSecond (BRateLimit _ DAY    limit) = fromRational (fromIntegral limit % 3600*24)

instance Json.FromJSON BRateLimit

data LimitType = REQUEST_WEIGHT | ORDERS deriving (Eq, Generic)
instance Json.FromJSON LimitType

data LimitInterval = SECOND | MINUTE | DAY deriving (Eq, Generic)
instance Json.FromJSON LimitInterval

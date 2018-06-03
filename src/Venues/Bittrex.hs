{-
   https://bittrex.com/home/api
-}
module Venues.Bittrex
()
where

import CPrelude
import Prelude (getChar)
import OrderBook
import Fetch
import Types.Market

import qualified Servant.Client        as SC
import qualified Data.Scientific as Sci
import Servant.API
import qualified Data.Aeson   as Json
import qualified Data.Aeson.Types   as Json
import           Data.Vector  (Vector)
import Control.Monad.Fail
import qualified Data.Char as Char
import qualified Money


instance Json.FromJSON (SomeBook "bittrex") where
   parseJSON val =
      let fromBook Book{..} = mkSomeBook
            <$> traverse parseOrder buy
            <*> traverse parseOrder sell
      in Json.parseJSON val >>= fromBook . result >>= either fail return

newtype Wrap res = Wrap
   { result :: res
   } deriving Generic

instance Json.FromJSON res => Json.FromJSON (Wrap res)

data Book = Book
   { buy    :: Vector BittrexOrder
   , sell   :: Vector BittrexOrder
   } deriving Generic

instance Json.FromJSON Book

data BittrexOrder = BittrexOrder
   { quantity  :: Sci.Scientific
   , rate      :: Sci.Scientific
   } deriving (Show, Generic)

instance Json.FromJSON BittrexOrder where
  parseJSON  = Json.genericParseJSON
      (Json.defaultOptions { Json.fieldLabelModifier = firstCharUpper })

firstCharUpper :: String -> String
firstCharUpper [] = []
firstCharUpper (c1:cs) = Char.toUpper c1 : cs

parseOrder :: BittrexOrder -> Json.Parser SomeOrder
parseOrder bo@BittrexOrder{..} =
   maybe (fail $ "Bad BittrexOrder: " ++ show bo) return soM
   where soM = mkSomeOrder (toRational quantity) (toRational rate)


apiUrl = BaseUrl Https "bittrex.com" 443 ""

-- | Example: https://bittrex.com/api/v1.1/public/getorderbook?market=BTC-ADA&type=both
type Api base quote
   = "api"
   :> "v1.1"
   :> "public"
   :> "getorderbook"
   :> QueryParam "market" Text
   :> QueryParam "type" Text
   :> Get '[JSON] (SomeBook "bittrex")


mkBookSrc :: Text -> DataSrc (SomeBook "bittrex")
mkBookSrc pair = DataSrc apiUrl (clientM (Just pair) (Just "both"))
   where
   clientM = SC.client (Proxy :: Proxy (Api base quote))

data BMarket = BMarket
   { marketCurrency :: Text
   , baseCurrency   :: Text
   , marketName     :: Text
   , isActive       :: Bool
   } deriving Generic

instance Json.FromJSON BMarket where
  parseJSON = Json.genericParseJSON
      Json.defaultOptions { Json.fieldLabelModifier = firstCharUpper }

-- | https://bittrex.com/api/v1.1/public/getmarkets
type ApiMarkets
   = "api"
   :> "v1.1"
   :> "public"
   :> "getmarkets"
   :> Get '[JSON] (MarketList "bittrex")

instance MarketBook "bittrex" where
   marketBook = mkBookSrc
   rateLimit = DataSrc apiUrl (return . fromRational . toRational $ 1)
   -- Rate limit: 1 per second (https://bitcoin.stackexchange.com/questions/59316/trading-bot-what-is-the-maximum-load-an-exchange-server-can-take)

instance EnumMarkets "bittrex" where
   allMarkets = DataSrc apiUrl clientM
      where
         clientM = SC.client (Proxy :: Proxy ApiMarkets)

instance Json.FromJSON (MarketList "bittrex") where
   parseJSON val = do
      wrap <- Json.parseJSON val
      return $ MarketList $ map fromBM (result wrap)

--instance Json.FromJSON (Market "bittrex") where

fromBM :: BMarket -> Market venue
fromBM BMarket{..} =
   Market   -- Bittrex swaps around base/quote currency:
            --  https://twitter.com/runeksvendsen/status/945713209406902272
      { miBase       = marketCurrency
      , miQuote      = baseCurrency
      , miApiSymbol  = marketName
      }

{-
    {
	"success" : true,
	"message" : "",
	"result" : {
		"buy" : [{
				"Quantity" : 12.37000000,
				"Rate" : 0.02525000
			}
		],
		"sell" : [{
				"Quantity" : 32.55412402,
				"Rate" : 0.02540000
			}, {
				"Quantity" : 60.00000000,
				"Rate" : 0.02550000
			}, {
				"Quantity" : 60.00000000,
				"Rate" : 0.02575000
			}, {
				"Quantity" : 84.00000000,
				"Rate" : 0.02600000
			}
		]
	}
}

 -}


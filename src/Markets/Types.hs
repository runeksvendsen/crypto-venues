module Markets.Types where

import CPrelude
import Fetch.DataSrc
import OrderBook.Types
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import           Servant.API
import qualified Data.Aeson            as Json
import qualified Network.HTTP.Client   as HTTP


data Market (venue :: Symbol) = Market
   { miBase       :: Text
   , miQuote      :: Text
   , miApiSymbol  :: Text
   } deriving (Eq, Generic)

newtype MarketList venue = MarketList [Market venue]


instance KnownSymbol venue => Show (Market venue) where
   show Market{..} = printf template venueName miBase miQuote
      where
         template = "<%s %s/%s>"
         venueName = symbolVal (Proxy :: Proxy venue)

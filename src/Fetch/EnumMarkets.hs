module Fetch.EnumMarkets where

import CPrelude
import Fetch.DataSrc
import Types.RateLimit
import OrderBook.Types
import Types.Market
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import           Servant.API
import qualified Data.Aeson            as Json
import qualified Network.HTTP.Client   as HTTP


-- | Enumerate all available markets for a given venue
class KnownSymbol venue => EnumMarkets venue where
   allMarkets :: DataSrc (MarketList venue)  -- ^

marketList
   :: forall venue m.
      (EnumMarkets venue, MonadIO m)
   => Proxy venue
   -> AppM m [Market venue]
marketList p = do
   man <- ask
   res :: MarketList venue <- throwLeft =<< srcFetch man allMarkets
   return $ getMarkets res

module CryptoVenues.Fetch.EnumMarkets where

import CryptoVenues.Internal.CPrelude
import CryptoVenues.Fetch.DataSrc
import CryptoVenues.Types.RateLimit
import OrderBook.Types
import CryptoVenues.Types.Market
import qualified Servant.Client.Core.Reexport as S
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
   man <- asks cfgMan
   let handleErr = throwLeft . fmapL (Error (VenueEnumErr p))
   res :: MarketList venue <- handleErr =<< srcFetch man allMarkets
   return $ getMarkets res
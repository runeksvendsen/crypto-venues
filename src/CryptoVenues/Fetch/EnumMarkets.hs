module CryptoVenues.Fetch.EnumMarkets where

import CryptoVenues.Internal.Prelude
import CryptoVenues.Types.AppM.Internal
import CryptoVenues.Fetch.DataSrc
import qualified CryptoVenues.Internal.Log    as Log
import CryptoVenues.Types.Market
import qualified Servant.Client        as SC


-- | Enumerate all available markets for a given venue
class KnownSymbol venue => EnumMarkets venue where
   allMarkets :: DataSrc (MarketList venue)  -- ^
   apiQuirk   :: Proxy venue
              -> SC.ServantError
              -> IO SC.ServantError          -- ^ Circumvent API quirk by modifying error response
                                             --    before the error is handled generically
   apiQuirk _ = return

marketList
   :: forall venue m.
      (EnumMarkets venue, MonadIO m)
   => AppM m [Market venue]
marketList = do
   man <- asks cfgMan
   let handleErr = throwLeft . fmapL (Error (VenueEnumErr p))
       p = Proxy :: Proxy venue
   res :: MarketList venue <- handleErr =<< liftIO (srcFetch man allMarkets (apiQuirk p))
   logMarketListFetch res
   return $ getMarkets res
  where
   logMarketListFetch = lift . Log.log' . toS . mkLogMessage
   mkLogMessage :: MarketList venue -> String
   mkLogMessage (MarketList markets) =
      printf "%s: Fetched %d markets" venueName (length markets)
   venueName = symbolVal (Proxy :: Proxy venue)

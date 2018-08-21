module CryptoVenues.Fetch.Throttle
( fetchRateLimited )
where

import CryptoVenues.Internal.Prelude
import CryptoVenues.Types.AppM.Internal
import CryptoVenues.Types.Market
import CryptoVenues.Fetch.MarketBook
import OrderBook.Types
import qualified Control.RateLimit  as Lim
import qualified CryptoVenues.Internal.Log as Log


-- | Fetch a number of order books as per the venue's rate limit,
--    with built-in retrying in case of request failure (e.g. 'TooManyRequests')
fetchRateLimited
   :: forall venue.
      MarketBook venue
   => [Market venue]
   -> AppM IO [AnyBook venue]
fetchRateLimited marketLst = do
   limFetch <- mkRateLimited
   handleErr $ foldrM (go limFetch) (Right []) marketLst
   where
      handleErr :: IO (Either Error a) -> AppM IO a
      handleErr ioa = throwLeft =<< liftIO ioa

-- | Fetch a single 'AnyBook' with rate-limiting enabled, and
--  recover from failure as per 'retryPolicy'
go :: KnownSymbol venue
   => (Market venue -> IO (Either Error (AnyBook venue)))   -- ^ Rate-limited fetch function
   -> Market venue                                          -- ^ Which order book to fetch?
   -> Either Error [AnyBook venue]                          -- ^ Result of previous fetch (error or list of order books)
   -> IO (Either Error [AnyBook venue])
go limFetch market (Right lst) = do
      resE <- limFetch market
      return $ fmap (: lst) resE
go _ _ left = return left

-- | Create rate-limited order book fetch-function
mkRateLimited
   :: forall venue.
      MarketBook venue
   => AppM IO (Market venue -> IO (Either Error (AnyBook venue)))
mkRateLimited = do
   limit :: RateLimit venue <- getRateLimit
   -- NB: Per-execution rate limit is important here,
   --  because otherwise new requests would be spawned
   --  while existing requests, that are paused while retrying,
   --  would be waiting to finish.
   cfg <- ask
   liftIO $ Lim.rateLimitExecution limit (runAppM cfg . fetchBook)
   where
      fetchBook :: Market venue -> AppM IO (AnyBook venue)
      fetchBook = fetchMarketBook

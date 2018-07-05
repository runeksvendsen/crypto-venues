{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CryptoVenues.Fetch.Throttle
( fetchRateLimited )
where

import CryptoVenues.Internal.CPrelude
import CryptoVenues.Types.Market
import CryptoVenues.Fetch.MarketBook
import CryptoVenues.Fetch.DataSrc
import CryptoVenues.Fetch
import CryptoVenues.Types.RateLimit
import OrderBook.Types
import Control.Monad
import qualified Control.Retry      as Re
import qualified Control.RateLimit  as Lim
import qualified Data.Time.Units    as Time
import qualified CryptoVenues.Internal.Log as Log


-- | Maximum number of retries
numMaxRetries :: Int
numMaxRetries = 5

retryPolicy :: RateLimit venue -> Re.RetryPolicyM IO
retryPolicy limit =
   Re.exponentialBackoff backoffMicroSecs
   <> Re.limitRetries numMaxRetries
      where backoffMicroSecs = fromIntegral $ Time.toMicroseconds limit

-- | Fetch a number of order books as per the venue's rate limit,
--    with built-in retrying in case of request failure (e.g. 'TooManyRequests')
fetchRateLimited
   :: forall venue.
      MarketBook venue
   => [Market venue]
   -> AppM IO [AnyBook venue]
fetchRateLimited marketLst = do
   (limFetch, limit) <- mkRateLimited
   handleErr $ foldrM (go limit limFetch) (Right []) marketLst
   where
      handleErr :: IO (Either Error a) -> AppM IO a
      handleErr ioa = throwLeft =<< liftIO ioa

-- | Fetch a single 'AnyBook' with rate-limiting enabled, and
--  recover from failure as per 'retryPolicy'
go :: KnownSymbol venue
   => RateLimit venue
   -> (Market venue -> IO (Either Error (AnyBook venue)))   -- ^ Rate-limited fetch function
   -> Market venue                                          -- ^ Which order book to fetch?
   -> Either Error [AnyBook venue]                          -- ^ Result of previous fetch (error or list of order books)
   -> IO (Either Error [AnyBook venue])
go limit limFetch market (Right lst) =
   Re.retrying (retryPolicy limit) doRetry $ \_ -> do
      resE <- Log.timedLogEnd (show' market <> " order book fetched.") $
         limFetch market
      return $ fmap (: lst) resE
go _ _ _ left = return left

-- | Should we retry an error or not? (also logs)
doRetry
   :: forall venue.
      KnownSymbol venue
   => Re.RetryStatus
   -> Either Error [AnyBook venue]
   -> IO Bool
doRetry _                  (Right _)  = return False
doRetry Re.RetryStatus{..} (Left err) = do
   let retrying = shouldRetry err
       retryStr = if retrying then "Retrying " else "Not retrying "
       attempt  = " (attempt " <> show' rsIterNumber <> ")"
       logFun = if retrying then Log.warnS else Log.lerrorS
   logFun ("Fetch" <> attempt) $ retryStr <> "failed request: " <> show' err
   return retrying

-- | Create rate-limited order book fetch-function
mkRateLimited
   :: forall venue.
      MarketBook venue
   => AppM IO (Market venue -> IO (Either Error (AnyBook venue)), RateLimit venue)
mkRateLimited = do
   man <- asks cfgMan
   let venue = Proxy :: Proxy venue
       handleErr = throwLeft . fmapL (Error (VenueEnumErr venue))
   limit :: RateLimit venue <- handleErr =<< srcFetch man dSrc
   liftIO $ Log.infoS (toS $ symbolVal venue) $ "Rate limit: " <> show' limit
   -- NB: Per-execution rate limit is important here,
   --  because otherwise new requests would be spawned
   --  while existing requests, that are paused while retrying,
   --  would be waiting to finish.
   limFun <- liftIO $ Lim.rateLimitExecution limit (runAppM man . fetchBook)
   return (limFun, limit)
   where
      dSrc :: DataSrc (RateLimit venue)
      dSrc = rateLimit
      fetchBook :: Market venue -> AppM IO (AnyBook venue)
      fetchBook = fetchMarketBook

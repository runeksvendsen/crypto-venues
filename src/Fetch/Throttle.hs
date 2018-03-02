{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fetch.Throttle
( fetchRateLimited )
where

import CPrelude
import Types.Market
import Fetch.MarketBook
import Fetch.DataSrc
import Fetch
import Types.RateLimit
import OrderBook.Types
import Control.Monad
import qualified Control.Retry      as Re
import qualified Control.RateLimit  as Lim
import qualified Data.Time.Units    as Time


-- | Initial waiting time for exponential backoff
backoffStartWait :: Time.Millisecond
backoffStartWait = 500

-- | Maximum number of retries
numMaxRetries :: Int
numMaxRetries = 5

retryPolicy =
   Re.exponentialBackoff backoffMicroSecs
   <> Re.limitRetries numMaxRetries
      where backoffMicroSecs = fromIntegral $ Time.toMicroseconds backoffStartWait

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
      handleErr :: IO (Either FetchErr a) -> AppM IO a
      handleErr ioa = throwLeft =<< liftIO ioa

-- | Fetch a single 'AnyBook' with rate-limiting enabled, and
--  recover from failure as per 'retryPolicy'
go :: (Market venue -> IO (Either FetchErr (AnyBook venue)))   -- ^ Rate-limited fetch function
   -> Market venue                                             -- ^ Which order book to fetch?
   -> Either FetchErr [AnyBook venue]                          -- ^ Result of previous fetch (error or list of order books)
   -> IO (Either FetchErr [AnyBook venue])
go limFetch market (Right lst) =
   Re.retrying retryPolicy doRetry $ \_ -> do
      resE <- limFetch market
      return $ fmap (: lst) resE
go _ _ left = return left

-- | Should we retry an error or not?
doRetry _ (Left err) = return $ shouldRetry err
doRetry _ _ = return False

-- | Create rate-limited order book fetch-function
mkRateLimited
   :: forall venue.
      MarketBook venue
   => AppM IO (Market venue -> IO (Either FetchErr (AnyBook venue)))
mkRateLimited = do
   man <- ask
   limit :: RateLimit venue <- throwLeft =<< srcFetch man dSrc
   -- NB: Per-execution rate limit is important here,
   --  because otherwise new requests would be spawned
   --  while existing requests, that are paused while retrying,
   --  would be waiting to finish.
   liftIO $ Lim.rateLimitExecution limit (runAppM man . fetchBook)
   where
      dSrc :: DataSrc (RateLimit venue)
      dSrc = rateLimit
      fetchBook :: Market venue -> AppM IO (AnyBook venue)
      fetchBook = fetchMarketBook

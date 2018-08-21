{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeFamilies #-}
module CryptoVenues.Internal.RateLimitCache
( RateLimitCache
, create
, insert
, lookup
-- * Re-exports
, RateLimit
)
where

import CryptoVenues.Internal.Prelude hiding (lookup)
import CryptoVenues.Types.RateLimit

import qualified Data.Cache         as Cache
import qualified System.Clock       as Clock


data SomeRateLimit
    = forall venue. KnownSymbol venue
    => SomeRateLimit (RateLimit venue)

type RateLimitCache = Cache.Cache Text SomeRateLimit

timeout :: Clock.TimeSpec
timeout = Clock.fromNanoSecs (1e9 * 3600 * 24)

create :: MonadIO m => m RateLimitCache
create = liftIO $ Cache.newCache (Just timeout)

insert :: forall m venue.
          (MonadIO m, KnownSymbol venue)
       => RateLimitCache
       -> RateLimit venue
       -> m ()
insert cache rl = liftIO $
    Cache.insert cache (toS $ symbolVal (Proxy :: Proxy venue)) (SomeRateLimit rl)

lookup :: forall m venue.
          (MonadIO m, KnownSymbol venue)
       => RateLimitCache
       -> m (Maybe (RateLimit venue))
lookup cache = do
    someRlM <- liftIO $ Cache.lookup cache (toS key)
    return $ handleSomeRL <$> someRlM
  where
    key = symbolVal (Proxy :: Proxy venue)
    handleSomeRL (SomeRateLimit rl) = handleRL rl
    handleRL :: forall venueMap.
                KnownSymbol venueMap
             => RateLimit venueMap
             -> RateLimit venue
    handleRL rl =
        case sameSymbol (Proxy :: Proxy venue) (Proxy :: Proxy venueMap) of
            Just Refl -> rl
            Nothing   -> error $ "BUG: RateLimit under key " ++ show key
                                ++ "is not " ++ symbolVal (Proxy :: Proxy venueMap)

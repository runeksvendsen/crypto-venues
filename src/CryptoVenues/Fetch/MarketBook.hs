{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module CryptoVenues.Fetch.MarketBook
( MarketBook(..)
, AnyMarket(..)
, DataSrc(..)
, RateLimit
, fetchMarketBook
, getRateLimit
, AnyVenue(..)
, marketListAny
)
where

import CryptoVenues.Internal.Prelude
import CryptoVenues.Types.AppM.Internal
import CryptoVenues.Fetch.DataSrc
import CryptoVenues.Fetch.EnumMarkets
import CryptoVenues.Types.RateLimit
import OrderBook.Types
import CryptoVenues.Types.Market
import qualified Data.Aeson            as Json
import qualified Control.Retry                              as Re
import qualified CryptoVenues.Internal.Log                  as Log
import qualified Data.Time.Units                            as Time
import qualified CryptoVenues.Internal.RateLimitCache       as Cache
import qualified Control.RateLimit                          as Lim


-- | Order book fetching
class EnumMarkets venue => MarketBook venue where
   marketBook :: Text -> DataSrc (SomeBook venue)  -- ^ Fetch a 'SomeBook' given a 'Market' 'miApiSymbol'
   rateLimit  :: DataSrc (RateLimit venue)         -- ^ How often can we fetch using 'marketBook'?

-- |
data AnyMarket =
   forall venue.
   MarketBook venue
      => AnyMarket { anyMarket :: Market venue }

instance NFData AnyMarket where
   rnf (AnyMarket mkt) = rnf mkt

instance Eq AnyMarket where
   (AnyMarket (m1 :: Market venue1)) == (AnyMarket (m2 :: Market venue2)) =
         miBase m1 == miBase m2
      && miQuote m1 == miQuote m2

instance Show AnyMarket where
   show (AnyMarket m) = show m

instance Hashable AnyMarket where
   hashWithSalt salt (AnyMarket mkt) =
      hashWithSalt salt [miBase mkt, miQuote mkt]

getRateLimit
      :: forall m venue.
         (MonadIO m, MarketBook venue)
      => AppM m (Cache.RateLimit venue)
getRateLimit = do
    rlCache <- asks cfgRateLimitCache
    maybe (fetchStore rlCache) return =<< Cache.lookup rlCache
  where
    fetchStore :: MonadIO m => Cache.RateLimitCache -> AppM m (Cache.RateLimit venue)
    fetchStore cache = do
        man <- asks cfgMan
        let venue = Proxy :: Proxy venue
            handleErr = throwLeft . fmapL (Error (VenueEnumErr venue))
        limit <- handleErr =<< liftIO (srcFetch man rateLimit (apiQuirk venue))
        liftIO $ Log.infoS (toS $ symbolVal venue) $ "Fetched rate limit: " <> show' limit
        Cache.insert cache limit
        return limit

fetchMarketBook
   :: forall venue.
      MarketBook venue
   => Market venue
   -> AppM IO (AnyBook venue)
fetchMarketBook market = do
    cfg <- ask
    limit :: RateLimit venue <- getRateLimit
    fetchRL <- liftIO $ Lim.rateLimitExecution limit (runAppM cfg . fetchMarketBook')
    throwLeft =<< liftIO (fetchRL market)

fetchMarketBook'
   :: forall venue.
      MarketBook venue
   => Market venue
   -> AppM IO (AnyBook venue)
fetchMarketBook' market@Market{..} = do
   man <- asks cfgMan
   maxRetries <- asks cfgNumMaxRetries
   limit :: RateLimit venue <- getRateLimit
   let handleErr = fmapL (Error (BookErr market))
       proxy = Proxy :: Proxy venue
   resE <- Re.retrying (retryPolicy maxRetries limit) doRetry $ \_ ->
      handleErr <$> liftIO (srcFetch man (marketBook miApiSymbol) (apiQuirk proxy))
   ob <- throwLeft resE
   liftIO $ Log.infoS (toS $ symbolVal proxy) $ show' market <> " order book fetched"
   return $ bookFromMarket market ob

-- | Should we retry an error or not? (also logs)
doRetry
   :: Re.RetryStatus
   -> Either Error a
   -> AppM IO Bool
doRetry _                  (Right _)  = return False
doRetry Re.RetryStatus{..} (Left err) = do
   let retrying = shouldRetry err
       retryStr = if retrying then "Retrying " else "Not retrying "
       attempt  = " (attempt " <> show' rsIterNumber <> ")"
       logFun = if retrying then Log.warnS else Log.lerrorS
   liftIO $ logFun ("Fetch" <> attempt) $ retryStr <> "failed request: " <> show' err
   return retrying

retryPolicy :: Word -> RateLimit venue -> Re.RetryPolicyM (AppM IO)
retryPolicy numMaxRetries limit =
   Re.fullJitterBackoff backoffMicroSecs
   <> Re.limitRetries (fromIntegral numMaxRetries)
      where backoffMicroSecs = fromIntegral $ Time.toMicroseconds limit

-- | Convert a 'SomeBook' into an 'AnyBook' given a 'Market'
bookFromMarket
   :: forall venue.
      KnownSymbol venue
   => Market venue
   -> SomeBook venue
   -> AnyBook venue
bookFromMarket market sb =
   case someSymbolVal (toS $ miBase market) of
      SomeSymbol (Proxy :: Proxy base) ->
         case someSymbolVal (toS $ miQuote market) of
               SomeSymbol (Proxy :: Proxy quote) ->
                  AnyBook (fromSomeBook sb :: OrderBook venue base quote)

-- |
data AnyVenue
   = forall venue.
     MarketBook venue
   => AnyVenue (Proxy venue)

instance Show AnyVenue where
   show (AnyVenue p) = symbolVal p

instance Json.ToJSON AnyVenue where
   toJSON (AnyVenue p) = Json.toJSON (toS $ symbolVal p :: Text)

marketListAny
   :: MonadIO m
   => AnyVenue
   -> AppM m [AnyMarket]
marketListAny (AnyVenue (_ :: Proxy venue)) =
   map (AnyMarket :: Market venue -> AnyMarket) <$> marketList

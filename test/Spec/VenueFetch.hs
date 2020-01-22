module Spec.VenueFetch
( spec )
where

import CryptoVenues.Internal.CPrelude
import qualified CryptoVenues.Venues as Venues
import           Spec.Orphans ()
import qualified Spec.RateLimit as RateLimit
import CryptoVenues.Fetch

import CryptoVenues.Types.Market
import Test.Hspec
import qualified Network.HTTP.Client   as HTTP


-- | A sane lower bound on how many markets a venue must have
minNumMarkets :: Int
minNumMarkets = 5

spec :: HTTP.Manager -> Word -> Spec
spec man maxRetries = parallel $
   forM_ allVenuesBitfinexRaw (testVenue man maxRetries)
  where
   allVenuesBitfinexRaw = AnyVenue (Proxy :: Proxy "bitfinex-raw") : Venues.allVenues

testVenue :: HTTP.Manager -> Word -> AnyVenue -> Spec
testVenue man maxRetries av@(AnyVenue (_ :: Proxy venue)) =
   around (withMarketList man maxRetries :: ([Market venue] -> IO a) -> IO a) $
      describe ("for " ++ show av) $
         parallel $ do
            testMarketListLength
            RateLimit.testRateLimitFetch man maxRetries

testMarketListLength :: SpecWith (Arg ([Market venue] -> IO ()))
testMarketListLength =
   it ("we can fetch at least " ++ show minNumMarkets ++ " markets") $ \markets ->
      length markets `shouldSatisfy` (>= minNumMarkets)

withMarketList
   :: forall venue a. EnumMarkets venue
   => HTTP.Manager
   -> Word
   -> ([Market venue] -> IO a)
   -> IO a
withMarketList man maxRetries f = do
   markets <- failOnErr =<< runAppM man maxRetries marketList
   f markets

failOnErr :: (Monad m, Show e) => Either e a -> m a
failOnErr = either (error . show) return

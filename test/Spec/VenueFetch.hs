module Spec.VenueFetch
( spec )
where

import CPrelude
import qualified Venues
import qualified Spec.RateLimit as RateLimit
import Fetch

import Types.Market
import Fetch
import Test.Hspec
import qualified Network.HTTP.Client   as HTTP
import qualified Test.QuickCheck    as QC


-- | A sane lower bound on how many markets a venue must have
minNumMarkets :: Int
minNumMarkets = 5

spec :: HTTP.Manager -> Spec
spec man =
   forM_ Venues.allVenues (testVenue man)

testVenue :: HTTP.Manager -> AnyVenue -> Spec
testVenue man av@(AnyVenue venue) = parallel $
   around (withMarketList man venue) $
      describe ("for " ++ show av) $ do
         testMarketListLength man
         testFetchArbOrderbook man
         RateLimit.testRateLimitFetch man

testMarketListLength :: HTTP.Manager -> SpecWith (Arg ([Market venue] -> IO ()))
testMarketListLength man =
   it ("we can fetch at least " ++ show minNumMarkets ++ " markets") $ \markets ->
      length markets `shouldSatisfy` (>= minNumMarkets)

testFetchArbOrderbook
   :: MarketBook venue
   => HTTP.Manager
   -> SpecWith (Arg ([Market venue] -> IO ()))
testFetchArbOrderbook man =
   it "we can fetch a randomly chosen market order book" $ \markets -> do
      market <- QC.generate (QC.elements markets)
      bookE <- runAppM man $ fetchMarketBook market
      bookE `shouldSatisfy` isRight

withMarketList
   :: forall venue. EnumMarkets venue
   => HTTP.Manager
   -> Proxy venue
   -> ([Market venue] -> IO ()) -> IO ()
withMarketList man venue f = do
   markets <- failOnErr =<< runAppM man (marketList venue)
   f markets

failOnErr :: (Monad m, Show e) => Either e a -> m a
failOnErr = either (error . show) return

module Spec.VenueFetch where

import CPrelude
import qualified Venues
import Fetch
import Venue.Types
import Markets.Types
import Markets.Fetch
import Test.Hspec
import qualified Network.HTTP.Client   as HTTP
import qualified Test.QuickCheck    as QC


-- | The minimum number of markets we require a venue to have
minNumMarkets :: Int
minNumMarkets = 5

spec :: HTTP.Manager -> Spec
spec man =
   forM_ Venues.allVenues (testVenue man)

testVenue :: HTTP.Manager -> AnyVenue -> Spec
testVenue man venue =
   around (withMarketList man venue) $
      parallel $
         describe ("for " ++ show venue) $ do
            testMarketListLength man
            testFetchArbOrderbook man

testMarketListLength :: HTTP.Manager -> SpecWith (Arg ([AnyMarket] -> IO ()))
testMarketListLength man =
   it ("we can fetch at least " ++ show minNumMarkets ++ " markets") $ \markets ->
      length markets `shouldSatisfy` (>= minNumMarkets)

testFetchArbOrderbook :: HTTP.Manager -> SpecWith (Arg ([AnyMarket] -> IO ()))
testFetchArbOrderbook man =
   it "we can fetch a randomly chosen market orderbook" $ \markets -> do
      AnyMarket market <- QC.generate (QC.elements markets)
      bookE <- fetchMarketBook man market
      bookE `shouldSatisfy` isRight

withMarketList :: HTTP.Manager -> AnyVenue -> ([AnyMarket] -> IO ()) -> IO ()
withMarketList man venue f = do
   markets <- liftIO $ failOnErr =<< runExceptT (marketList man venue)
   f markets

failOnErr :: (Monad m, Show e) => Either e a -> m a
failOnErr = either (error . show) return

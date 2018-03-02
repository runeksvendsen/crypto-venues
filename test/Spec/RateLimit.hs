module Spec.RateLimit
( testRateLimitFetch )
where

import CPrelude
import qualified Fetch.Throttle as Throttle
import qualified Venues
import Types.Market
import Fetch
import Test.Hspec
import qualified Network.HTTP.Client   as HTTP
import qualified Test.QuickCheck    as QC


numOrderbooks :: Int
numOrderbooks = 50

testRateLimitFetch
   :: MarketBook venue
   => HTTP.Manager
   -> SpecWith (Arg ([Market venue] -> IO ()))
testRateLimitFetch man =
   it ("we can do rate-limited fetch of " ++ show numOrderbooks ++ " order books") $ \markets -> do
      someMarkets <- QC.generate (QC.shuffle markets)
      let marketLst = take numOrderbooks someMarkets
      bookLstE <- runAppM man $ Throttle.fetchRateLimited marketLst
      bookLstE `shouldSatisfy` isRight
      let Just bookLst = rightToMaybe bookLstE
      return $ rnf bookLst
      length bookLst `shouldBe` length marketLst

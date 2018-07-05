module Spec.RateLimit
( testRateLimitFetch, rateLimitFetch )
where

import CryptoVenues.Internal.CPrelude
import qualified CryptoVenues.Fetch.Throttle as Throttle
import CryptoVenues.Types.Market
import CryptoVenues.Fetch
import Test.Hspec
import qualified Network.HTTP.Client   as HTTP
import qualified Test.QuickCheck    as QC


numOrderbooks :: Int
numOrderbooks = 50

testRateLimitFetch
   :: MarketBook venue
   => HTTP.Manager
   -> Word
   -> SpecWith (Arg ([Market venue] -> IO ()))
testRateLimitFetch man maxRetries =
   it ("we can do rate-limited fetch of " ++ show numOrderbooks ++ " order books") $
      rateLimitFetch man maxRetries

rateLimitFetch
   :: MarketBook venue
   => HTTP.Manager
   -> Word
   -> [Market venue]
   -> IO ()
rateLimitFetch man maxRetries markets = do
   someMarkets <- QC.generate (QC.shuffle markets)
   let marketLst = take numOrderbooks someMarkets
   bookLstE <- runAppM man maxRetries $ Throttle.fetchRateLimited marketLst
   bookLstE `shouldSatisfy` isRight
   let Just bookLst = rightToMaybe bookLstE
   return $ rnf bookLst
   length bookLst `shouldBe` length marketLst


module Spec.MarketString where

import CPrelude
import Markets          (fromString, toString)
import Fetch.MarketBook
import Orphans.Market

import Test.Hspec
import qualified Money
import qualified Test.QuickCheck    as QC
import qualified Text.Show.Pretty   as P
import qualified Test.Hspec.SmallCheck as SC
import qualified Test.SmallCheck.Series as SS


spec :: Spec
spec =
   describe "AnyMarket" $ do
      it "can be created from any well-formatted String" $
         SC.property $ \(SS.NonEmpty base) (SS.NonEmpty quote) (HyphenStr apiSym) ->
            fromString testVenue (toS $ base ++ "-" ++ quote ++ "-" ++ apiSym)
               `shouldSatisfy` isJust
      it "can be converted to/from any String" $
         SC.property $ \anyMarket ->
            fromString testVenue (toS $ toString anyMarket) `shouldBe` Just anyMarket

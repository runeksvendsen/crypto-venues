import CPrelude

import qualified Spec.MarketString  as MarketStr
import Markets                      (fromString, toString)

import Test.Tasty
import Test.Tasty.SmallCheck  as SC
import Test.Hspec             as HS
import Test.Hspec.Runner
import qualified Test.SmallCheck.Series as SS
import Orphans.Market

scDepth = 4

main = do
   defaultMain properties
--   hspecWith defaultConfig { configSmallCheckDepth = scDepth } MarketStr.spec


properties :: TestTree
properties = localOption (SC.SmallCheckDepth scDepth) $
   testGroup "Properties" [scProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "AnyMarket can be created from any String" $
      \(SS.NonEmpty base) (SS.NonEmpty quote) (HyphenStr apiSym) ->
         fromString testVenue (toS $ base ++ "-" ++ quote ++ "-" ++ apiSym)
               `shouldSatisfy` isJust
  , SC.testProperty "AnyMarket can be converted to/from any String" $
      \anyMarket ->
         fromString testVenue (toS $ toString anyMarket) `shouldBe` Just anyMarket
  ]

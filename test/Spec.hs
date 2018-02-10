import CPrelude

import qualified Spec.MarketString
import qualified Spec.VenueFetch
import Markets                      (fromString, toString)

import Test.Tasty
import Test.Tasty.SmallCheck  as SC
import Test.Hspec             as HS
import Test.Hspec.Runner
import qualified Test.SmallCheck.Series as SS
import Orphans.Market
import qualified Network.HTTP.Client.TLS as HTTPS


scDepth = 4

main = do
   man <- HTTPS.newTlsManager
   let runHspec = hspecWith defaultConfig { configSmallCheckDepth = scDepth }
   runHspec $ parallel $ do
      Spec.VenueFetch.spec man
      Spec.MarketString.spec
   --   defaultMain properties


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

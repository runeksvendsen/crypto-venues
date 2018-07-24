module Main where

import CryptoVenues.Internal.CPrelude
import qualified Spec.VenueFetch

import Test.Tasty
import Test.Tasty.SmallCheck  as SC
import Test.Hspec             as HS
import Test.Hspec.Runner
import qualified Test.SmallCheck.Series as SS
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified CryptoVenues.Internal.Log as Log


scDepth = 4
maxRetries = 10

main = Log.withStderrLogging $ do
   Log.setLogLevel Log.LevelError
   man <- HTTPS.newTlsManager
   let runHspec = hspecWith defaultConfig { configSmallCheckDepth = scDepth }
   runHspec $ Spec.VenueFetch.spec man maxRetries

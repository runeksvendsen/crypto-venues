module Main where

import CryptoVenues.Internal.CPrelude
import qualified Spec.VenueFetch
import qualified Spec.RetryAfter

import Test.Hspec.Runner
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified CryptoVenues.Internal.Log as Log


scDepth = 4
maxRetries = 10

main = Log.withStderrLogging $ do
   Log.setLogLevel Log.LevelError
   man <- HTTPS.newTlsManager
   let runHspec = hspecWith defaultConfig { configSmallCheckDepth = Just scDepth }
   runHspec $ Spec.RetryAfter.spec man
   runHspec $ Spec.VenueFetch.spec man maxRetries

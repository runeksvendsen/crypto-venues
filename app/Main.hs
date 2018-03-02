module Main where

import CPrelude
import qualified Paths

--import OrderBook
import Venues ()
import qualified Network.HTTP.Client.TLS as HTTPS

main :: IO ()
main = do
   man <- HTTPS.newTlsManager
   either (error . show) return =<< runAppM man Paths.main

module Main where

import CPrelude
import OrderBook
--import qualified OrderBook.Output as LibOut
--import qualified Api.Docs
--import Api.Handler
import qualified Servant.Server as SS
import Venues ()
import qualified Data.Aeson as Json
import qualified Network.HTTP.Client.TLS as HTTPS


-- getMarkets

main :: IO ()
main = do
   man <- HTTPS.newTlsManager
   return ()


sellBuy :: (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
        => OrderBook venue base quote
        -> IO ()
sellBuy book = do
   let quoteQty = 1e5
       slipPct  = 1
--   putStr ("BUY:  " :: Text)
--   putStrLn $ Lib.marketBuy book (fromRational quoteQty)
--   putStr ("SELL: " :: Text)
--   putStrLn $ Lib.marketSell book (fromRational quoteQty)
   putStr ("Slippage SELL: " :: Text)
   putStrLn $ slippageSell book (fromRational slipPct)
   putStr ("Slippage BUY:  " :: Text)
   putStrLn $ slippageBuy book (fromRational slipPct)
   putStrLn ("" :: Text)

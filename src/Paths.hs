{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
module Paths where

import CPrelude
import Prelude (unlines)
import qualified Paths.Paths as Paths
import Types.Market
import Fetch.MarketBook
import qualified Fetch.Throttle as Throttle
import qualified Fetch.EnumMarkets as EnumMarkets
import qualified Venues
import qualified Control.Monad.Parallel   as Par
import qualified Data.HashMap.Strict as Map


-- main = do
--     putStrLn test_fromLabeledNodes1
--     putStrLn test_fromLabeledNodes2
--     putStrLn test_fromLabeledNodes3
--     putStrLn test_fromLabeledNodes4

main :: AppM IO ()
main = do
    books <- allBooks
    let rateMap = Paths.buildRateMap books
        (depthGraph, nodeMap) = Paths.buildDepthGraph rateMap books
        lookupOrFail sym = fromMaybe (error $ "main: symbol not found: " ++ show sym) $
            Map.lookup sym nodeMap
        (btcNode, usdNode) = (lookupOrFail "BTC", lookupOrFail "USD")
    print "###### liquidPaths ######"
    mapM_ print $ take 25 $ Paths.liquidPaths rateMap nodeMap depthGraph btcNode usdNode


allBooks :: AppM IO [Paths.ABook]
allBooks =
   concat <$> Par.forM Venues.allVenues fetchVenueBooks

fetchVenueBooks
   :: AnyVenue
   -> AppM IO [Paths.ABook]
fetchVenueBooks (AnyVenue p) = do
    mktList :: [Market venue] <- EnumMarkets.marketList p
    let btcUsdL = filter (\mkt -> miBase mkt == "BTC" && miQuote mkt == "USD") mktList
        marketName = symbolVal (Proxy :: Proxy venue)
        markets = take 30 mktList
        marketList = case btcUsdL of
            []       -> markets
            [btcUsd] -> btcUsd : filter (/= btcUsd) markets
            _ -> error $ marketName ++ ": multiple BTC/USD markets"
    when (null btcUsdL) $
        putStrLn $ marketName ++ ": no BTCUSD market"
    map Paths.ABook <$> Throttle.fetchRateLimited marketList


{-

test_fromLabeledNodes1 = do
    let res = Paths.fromLabeledNodes [(1,"X"), (2,"e"), (3,"n"), (4,"u"), (5,"R")]
        exp = [(5,4,"R"), (4,3,"u"), (3,2,"n"), (2,1,"e")]
    if res == exp
        then "Success!"
        else unlines [ "Expected: " ++ show exp
                     , "Found: " ++ show res
                     ]

test_fromLabeledNodes2 = do
    let res = Paths.fromLabeledNodes [(1,"X"), (20,"BTC/USD")]
        exp = [(20,1,"BTC/USD")]
    if res == exp
        then "Success!"
        else unlines [ "Expected: " ++ show exp
                     , "Found: " ++ show res
                     ]

test_fromLabeledNodes3 = do
    let res = Paths.fromLabeledNodes [(20,"X"), (1,"BTC/USD")]
        exp = [(1,20,"BTC/USD")]
    if res == exp
        then "Success!"
        else unlines [ "Expected: " ++ show exp
                     , "Found: " ++ show res
                     ]


test_fromLabeledNodes4 = do
    let res = Paths.fromLabeledNodes [(20,"X"), (1,"BTC/USD"), (15,"RDD/BTC")]
        exp  = [(15,1,"RDD/BTC"), (1,20,"BTC/USD")]
        expR = [(1,20,"BTC/USD"), (15,1,"RDD/BTC")]
    if res == exp
        then "Success!"
        else unlines [ "Expected: " ++ show exp
                     , "Found: " ++ show res
                     ]


-}
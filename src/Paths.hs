{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
module Paths where


import CPrelude
import Paths.Paths
import Types.Market
import Fetch.MarketBook
import OrderBook.Types
import qualified Fetch.Throttle as Throttle
import qualified Fetch.EnumMarkets as EnumMarkets
import qualified OrderBook.Matching as Match
import qualified Venues
import qualified Control.Monad.Parallel   as Par
import Data.List (nub, tail, last)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import qualified Money
import qualified Data.Vector  as Vec


main = do
    books <- allBooks
    print $ fmap realToFrac $ buildRateMap books
    return ()

allBooks :: AppM IO [ABook]
allBooks =
   concat <$> Par.forM Venues.allVenues fetchVenueBooks

fetchVenueBooks
   :: AnyVenue
   -> AppM IO [ABook]
fetchVenueBooks (AnyVenue p) = do
    mktList :: [Market venue] <- EnumMarkets.marketList p
    let btcUsdL = filter (\mkt -> miBase mkt == "BTC" && miQuote mkt == "USD") mktList
        marketName = symbolVal (Proxy :: Proxy venue)
    when (null btcUsdL) $
        putStrLn $ marketName ++ ": no BTCUSD market"
    map ABook <$> Throttle.fetchRateLimited (btcUsdL ++ take 10 mktList)



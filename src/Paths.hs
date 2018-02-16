module Paths where

import CPrelude
import Markets
import Fetch.MarketBook
import Venues
import Data.HashGraph.Strict              (Edge(..), mkGraph)
import Data.HashGraph.Algorithms          (pathTree)
import qualified Control.Monad.Parallel   as Par
import Data.List  hiding (map, intercalate)
import qualified Data.Text as T


toEdge :: AnyMarket -> Edge AnyMarket Text
toEdge am@(AnyMarket market) =
   Edge (miBase market) am (miQuote market)

allMarkets :: (Par.MonadParallel m, MonadIO m) => AppM m [AnyMarket]
allMarkets = do
   man <- ask
   allMarketsLst <- lift $ Par.forM allVenues (marketList man)
   return $ concat allMarketsLst

allEdges :: (Par.MonadParallel m, MonadIO m) => AppM m [Edge AnyMarket Text]
allEdges = do
   marketLst <- allMarkets
   return $ map toEdge marketLst

-- | Get a list of nodes along a path of non-cyclical edges
getNodes :: [Edge AnyMarket Text] -> [Text]
getNodes = nub . concatMap toVertices -- Get all vertices and remove duplicates
   where toVertices (Edge v1 _ v2) = [v1, v2]


--main :: (Par.MonadParallel m, MonadIO m) => AppM m ()
--main = do
--   edges <- allEdges
--   let nodes = getNodes edges
--       graph = mkGraph edges (getNodes edges)
--       -- All paths going from "USD"
--       paths = pathTree "USD" graph
--       -- Pretty path, e.g. "EUR->JPY->GBP"
--       prettyPath edgeL = T.intercalate "->" (getNodes edgeL)
----       audEndPath = (\(Edge _ _ dst) -> dst == "AUD") . last
--       edgeData (Edge _ ed _) = ed
--   forM_ paths (putStrLn . prettyPath)


module Paths where

import CPrelude
import Types.Market
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
   allMarketsLst <- Par.forM allVenues marketListAny
   return $ concat allMarketsLst

allEdges :: (Par.MonadParallel m, MonadIO m) => AppM m [Edge AnyMarket Text]
allEdges = do
   marketLst <- allMarkets
   let mktLst = map toEdge (nub marketLst)
   return mktLst

-- | Get a list of nodes along a path of non-cyclical edges
getNodes :: [Edge AnyMarket Text] -> [Text]
getNodes = nub . concatMap toVertices -- Get all vertices and remove duplicates
   where toVertices (Edge v1 _ v2) = [v1, v2]

invert :: Edge AnyMarket Text -> Edge AnyMarket Text
invert (Edge v1 am v2) = Edge v2 am v1

main :: (Par.MonadParallel m, MonadIO m) => AppM m ()
main = do
   edges <- allEdges
   putStrLn $ "Edges: " ++ show (length edges)
   putStrLn $ "Nodes: " ++ show (length $ getNodes edges)

--   let someEdges = edges -- take 400 edges
--       mkUndirected edgs = edgs ++ map invert edgs
--       graph = mkGraph (mkUndirected someEdges) (getNodes someEdges)
--        -- All paths going from "USD"
--       paths = pathTree "USD" graph
--        -- Pretty path, e.g. "EUR->JPY->GBP"
--       prettyPath edgeL = T.intercalate "->" (getNodes edgeL)
--       hasEnd node = (\(Edge _ _ dst) -> dst == node) . last
--       edgeData (Edge _ ed _) = ed
--       ethPaths = filter (hasEnd "ETH") paths
--   paths `deepseq` putStrLn ("DONE!" :: String)
--   forM_ paths (putStrLn . prettyPath)


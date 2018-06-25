{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}

module Paths.Paths
( module Paths.Types
, module Paths.Paths
)
where

import CPrelude hiding (head)
import Paths.Types
import OrderBook.Types
import qualified OrderBook.Matching as Match
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Graph.Inductive.Query.SP as G
import qualified Data.Graph.Inductive.Query.BFS as G
-- import qualified Data.Graph.Inductive.Query.MST as G
import qualified Data.Graph.Inductive.Internal.RootPath as G
import Data.List (init, last, tail, head)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.HashMap.Strict as Map
import qualified Money
import qualified Data.Vector  as Vec



-- Util

traceIt a = show a `trace` a

pathLabels
    :: G.LPath b
    -> [b]
pathLabels = map snd . G.unLPath

delAllLEdges
    :: (G.DynGraph gr, Eq b)
    => [G.LEdge b]
    -> gr a b
    -> gr a b
delAllLEdges edges =
    flip (foldr G.delAllLEdge) edges


--- #### Graph building #### ---

type Sym = Text                         -- ^ A currency symbol, e.g. "USD", "EUR", "BTC", "ETH" etc.
type NodeMap = Map.HashMap Sym Int
type GraphM m a = S.StateT NodeMap m a

buildGraph
    :: forall gr edgeLabel. (G.DynGraph gr, Show edgeLabel)
    => (NodeMap -> ABook -> [G.LEdge edgeLabel])
    -> [ABook]
    -> (gr Sym edgeLabel, NodeMap)
buildGraph toEdges books =
    let (edges, nodeMap) = S.runState (buildGraphM toEdges books) Map.empty
        nodes = sortOn fst $ map swap (Map.toList nodeMap)
    in (G.mkGraph (traceIt nodes) edges, nodeMap)

buildGraphM
    :: forall m edgeLabel. (Monad m, Show edgeLabel)
    => (NodeMap -> ABook -> [G.LEdge edgeLabel])
    -> [ABook]
    -> GraphM m [G.LEdge edgeLabel]
buildGraphM toEdges =
    foldrM insertBook []
  where
    symbolNode :: Sym -> NodeMap -> (G.LNode Sym, NodeMap)
    symbolNode symStr bimap =
            let nextNode = Map.size bimap in
            case Map.lookup symStr bimap of
                Nothing   -> ((nextNode, symStr), Map.insert symStr nextNode bimap)
                Just node -> ((node,     symStr), bimap)
    insertBook
        :: ABook
        -> [G.LEdge edgeLabel]
        -> GraphM m [G.LEdge edgeLabel]
    insertBook ab@(ABook (AnyBook ob)) edges = do
        bimap <- S.get
        let (baseSym, quoteSym) = (abBase ob, abQuote ob)
            (baseNode,  baseBimap)  = symbolNode baseSym bimap
            (quoteNode, quoteBimap) = symbolNode quoteSym baseBimap
        S.put quoteBimap
        return $ edges ++ toEdges quoteBimap ab


--- #### Graph queries #### ---

allPaths
    :: (G.Graph gr, Real b)
    => G.Node       -- ^ Start node
    -> gr a b
    -> [[G.LNode b]]
allPaths start g =
      map init
    $ filter (not . null)
    $ map (G.unLPath)
    $ G.lbft start g
    -- G.lbft: From source of Data.Graph.Inductive.Query.BFS:
    --
    -- -- Note that the label of the first node in a returned path is meaningless;
    -- -- all other nodes are paired with the label of their incoming edge.



--- #### Rate graph #### ---

type RateGraph = G.Gr Sym Rational
type USDRateMap = Map.HashMap Sym Rational

getRate :: USDRateMap -> Sym -> Maybe Rational
getRate = flip Map.lookup

toRate
    :: G.Graph gr
    => gr Sym Rational
    -> [G.LNode Rational]
    -> Maybe (Sym, Rational)
toRate _ [] = Nothing
toRate g nodes = Just
    (firstNodeLabel, rate)
  where
    rate = 1 / (foldr (*) (1%1) $ map snd nodes)
    firstNode = fst $ fromMaybe (error $ "toRate: empty node list") $ headMay nodes
    firstNodeLabel = fromMaybe (error $ "toRate: no such node in graph: " ++ show firstNode) $
        G.lab g firstNode

buildRateMap :: [ABook] -> USDRateMap
buildRateMap books =
    toRateMap (graph :: RateGraph) nodeMap
  where
    (graph, nodeMap) = buildGraph toRateEdges books

toRateMap
    :: (G.Graph gr, Show (gr Sym Rational))
    => gr Sym Rational
    -> NodeMap
    -> USDRateMap
toRateMap g nodeMap =
      insertRate ("USD", 1 % 1)     -- USD/USD exchange rate is 1
    $ foldr insertRate Map.empty
    $ catMaybes
    $ map (toRate g)
    $ allPaths (symNode "USD") g
  where
    insertRate (sym, rate) rateMap = Map.insert sym rate rateMap
    symNode sym = fromMaybe (error $ show sym ++  " not found in map") $
        Map.lookup sym nodeMap

toRateEdges
    :: NodeMap
    -> ABook
    -> [G.LEdge Rational]
toRateEdges symbolMap ab@(ABook anyBook@(AnyBook ob)) =
   catMaybes
       [ mkSellEdge <$> bestBidM    -- Consume bid: quote->base
       , mkBuyEdge  <$> bestAskM    -- Consume ask: base->quote
       ]
 where
   bestBidM = rationalPrice <$> bestBid ob
   bestAskM = rationalPrice <$> bestAsk ob
   -- Ex.: BTC/USD @ 6500 -> (BTC,USD,6500) (USD,BTC,1/6500)
   mkSellEdge bb = (baseNode, quoteNode, bb)
   mkBuyEdge  ba = (quoteNode, baseNode, 1 / ba)
   -- Util
   rationalPrice = Money.fromExchangeRate . oPrice
   baseNode = lookupOrFail (abBase ob)
   quoteNode = lookupOrFail (abQuote ob)
   lookupOrFail sym = fromMaybe (error $ "toEdges: symbol not found: " ++ show sym) $
        Map.lookup sym symbolMap



--- #### Depth graph #### ---

slippagePercent :: Rational
slippagePercent = 5 % 1

type DepthEdge = Pair (Maybe SomeSide) Rational
type DepthGraph = G.Gr Sym DepthEdge

buildDepthGraph :: USDRateMap -> [ABook] -> (DepthGraph, NodeMap)
buildDepthGraph rateMap books =
    buildGraph (toDepthEdges rateMap) books

toDepthEdges
    :: USDRateMap
    -> NodeMap
    -> ABook
    -> [G.LEdge DepthEdge]
toDepthEdges rateMap symbolMap ab@(ABook anyBook@(AnyBook ob)) =
   [ sellEdge rateMap symbolMap (obBids ob)
   , buyEdge  rateMap symbolMap (obAsks ob)
   ]

sellEdge
    :: forall venue base quote. 
       (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
    => USDRateMap
    -> NodeMap
    -> BuySide venue base quote
    -> G.LEdge DepthEdge
sellEdge rateMap symbolMap bs = 
    (baseNode, quoteNode, pairSell)
  where
    -- Node info
    quoteSym = abQuote bs
    baseNode = lookupOrFail (abBase bs) symbolMap
    quoteNode = lookupOrFail quoteSym symbolMap
    -- Edge info
    pairSell = Pair (Just . SomeSide . Left $ bs) 
                    (if sellQty == 0 then infinity else 1 / sellQty)
    sellQty = usdQuoteQty quoteSym rateMap $ 
        Match.slippageSell bs slippagePercent

buyEdge
    :: forall venue base quote. 
       (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
    => USDRateMap
    -> NodeMap
    -> SellSide venue base quote
    -> G.LEdge DepthEdge
buyEdge rateMap symbolMap ss =  
    (quoteNode, baseNode, pairBuy)
  where
    -- Nodes info
    quoteSym = abQuote ss
    baseNode = lookupOrFail (abBase ss) symbolMap
    quoteNode = lookupOrFail quoteSym symbolMap
    -- Edge info
    pairBuy  = Pair (Just . SomeSide . Right $ ss) 
                    (if buyQty == 0 then infinity else 1 / buyQty)
    buyQty  = usdQuoteQty quoteSym rateMap $ 
        Match.slippageBuy ss slippagePercent
   
toEdge
    :: USDRateMap
    -> NodeMap
    -> SomeSide
    -> G.LEdge DepthEdge
toEdge rateMap symbolMap (SomeSide (Left bs)) = sellEdge rateMap symbolMap bs
toEdge rateMap symbolMap (SomeSide (Right ss)) = buyEdge rateMap symbolMap ss

lookupRateFail sym rateMap = fromMaybe (error $ "rate not found: " ++ show sym) $
    getRate rateMap sym
lookupOrFail sym symbolMap = fromMaybe (error $ "node not found: " ++ show sym) $
    Map.lookup sym symbolMap
usdQuoteQty quoteSym rateMap matchRes = 
    lookupRateFail quoteSym rateMap * toRational (Match.resQuoteQty matchRes)

-- | Paths from given symbol to another given symbol in descending order of liquidity
liquidPaths rm nm dg f = reverse . liquidPathsR [[]] rm nm dg f

liquidPathsR
    :: [[G.LNode DepthEdge]]    -- ^ TODO
    -> USDRateMap
    -> NodeMap
    -> DepthGraph
    -> G.Node                   -- ^ From
    -> G.Node                   -- ^ To
    -> [[G.LNode DepthEdge]]    -- ^ List of From->To paths in ascending order of liquidity
liquidPathsR currPaths rateMap nodeMap g from to =
    if null (mostLiquidPath g)
        then currPaths
        else liquidPathsR (mostLiquidPath g : currPaths) rateMap nodeMap newGraph from to
  where
    mostLiquidPath :: G.Gr a DepthEdge -> [G.LNode DepthEdge]
    mostLiquidPath = G.unLPath . G.getLPath to . G.spTree from
    pathEdges :: G.LPath DepthEdge -> [G.LEdge DepthEdge]
    pathEdges = map (toEdge rateMap nodeMap) . catMaybes . map pFst . pathLabels
    newGraph = delAllLEdges (pathEdges . G.LP $ mostLiquidPath (traceIt g)) g

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}

module Paths.Paths where

import CPrelude
import Paths.Types
import Types.Market
import Fetch.MarketBook
import OrderBook.Types
import qualified Fetch.Throttle as Throttle
import qualified Fetch.EnumMarkets as EnumMarkets
import qualified OrderBook.Matching as Match
import qualified Venues
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Graph.Inductive.Query.SP as G
import qualified Data.Graph.Inductive.Query.BFS as G
import qualified Data.Graph.Inductive.Query.MST as G
import qualified Data.Graph.Inductive.Internal.RootPath as G
import qualified Control.Monad.Parallel   as Par
import Data.List (nub, tail, last, init)
import qualified Data.Text as T
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.HashMap.Strict as Map
import qualified Money
import qualified Data.Vector  as Vec


-- Util
traceIt a = show a `trace` a


--- #### Graph building #### ---

type NodeMap = Map.HashMap Text Int
type GraphM m a = S.StateT NodeMap m a

buildGraph
    :: forall gr edgeLabel. (G.DynGraph gr, Show edgeLabel)
    => (NodeMap -> ABook -> [G.LEdge edgeLabel])
    -> [ABook]
    -> (gr Text edgeLabel, NodeMap)
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
    symbolNode :: Text -> NodeMap -> (G.LNode Text, NodeMap)
    symbolNode symStr bimap =
            let nextNode = Map.size bimap in
            case Map.lookup symStr bimap of
                Nothing   -> ((nextNode, symStr), Map.insert symStr nextNode bimap)
                Just node -> ((node,     symStr), bimap)
    insertBook
        :: ABook
        -> [G.LEdge edgeLabel]
        -> GraphM m [G.LEdge edgeLabel]
    insertBook ab@(ABook anyBook) edges = do
        bimap <- S.get
        let (baseSym, quoteSym) = (abBase anyBook, abQuote anyBook)
            (baseNode,  baseBimap)  = symbolNode baseSym bimap
            (quoteNode, quoteBimap) = symbolNode quoteSym baseBimap
        S.put quoteBimap
        return $ edges ++ toEdges quoteBimap ab



--- #### Rate graph #### ---

type RateGraph = G.Gr Text Rational
type USDRateMap = Map.HashMap Text Rational

getRate :: USDRateMap -> Text -> Maybe Rational
getRate = flip Map.lookup

toRate
    :: G.Graph gr
    => gr Text Rational
    -> [G.LNode Rational]
    -> Maybe (Text, Rational)
toRate _ [] = Nothing
toRate g nodes = Just
    (firstNodeLabel, rate)
  where
    rate = 1 / (foldr (*) (1%1) $ map snd nodes)
    firstNode = fst $ fromMaybe (error $ "toRate: empty node list") $ head nodes
    firstNodeLabel = fromMaybe (error $ "toRate: no such node in graph: " ++ show firstNode) $
        G.lab g firstNode

buildRateMap :: [ABook] -> USDRateMap
buildRateMap books =
    toRateMap nodeMap (graph :: RateGraph)
  where
    (graph, nodeMap) = buildGraph toRateEdges books

toRateMap
    :: (G.Graph gr, Show (gr Text Rational))
    => gr Text Rational
    -> NodeMap
    -> USDRateMap
toRateMap g nodeMap =
      foldr insertRate Map.empty
    $ catMaybes
    $ map (toRate g)
    $ map traceIt
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
   bestBidM = rationalPrice <$> obBids ob Vec.!? 0
   bestAskM = rationalPrice <$> obAsks ob Vec.!? 0
   -- Ex.: BTC/USD @ 6500 -> (BTC,USD,6500) (USD,BTC,1/6500)
   mkSellEdge bestBid = (baseNode, quoteNode, bestBid)
   mkBuyEdge  bestAsk = (quoteNode, baseNode, 1 / bestAsk)
   -- Util
   rationalPrice = Money.fromExchangeRate . oPrice
   baseNode = lookupOrFail (abBase anyBook)
   quoteNode = lookupOrFail (abQuote anyBook)
   lookupOrFail sym = fromMaybe (error $ "toEdges: symbol not found: " ++ show sym) $
        Map.lookup sym symbolMap



--- #### Depth graph #### ---

slippagePercent :: Rational
slippagePercent = 5 % 1

type DepthEdge = Pair (Maybe ABook) Rational

toDepthEdges
    :: NodeMap
    -> USDRateMap
    -> ABook
    -> [G.LEdge DepthEdge]
toDepthEdges symbolMap rateMap ab@(ABook anyBook@(AnyBook ob)) =
   [ (baseNode, quoteNode, pairSell)
   , (quoteNode, baseNode, pairBuy)
   ]
 where
   quoteSym = abQuote anyBook
   baseNode = lookupOrFail (abBase anyBook)
   quoteNode = lookupOrFail quoteSym
   pairSell = Pair sellQty ab
   pairBuy  = Pair buyQty ab
   sellQty = usdQuoteQty $ Match.slippageSell ob slippagePercent
   buyQty  = usdQuoteQty $ Match.slippageBuy  ob slippagePercent
   usdQuoteQty matchRes = lookupRateFail quoteSym * Match.resQuoteQty matchRes
   lookupRateFail sym = fromMaybe (error $ "toEdges: rate not found: " ++ show sym) $
        getRate rateMap sym
   lookupOrFail sym = fromMaybe (error $ "toEdges: symbol not found: " ++ show sym) $
        Map.lookup symbolMap sym

abBase
    :: AnyBook venue
    -> Text
abBase (AnyBook ob) =
    case ob of
        (ob :: OrderBook venue base quote) ->
            toS $ symbolVal (Proxy :: Proxy base) :: Text

abQuote
    :: AnyBook venue
    -> Text
abQuote (AnyBook ob) =
    case ob of
        (ob :: OrderBook venue base quote) ->
            toS $ symbolVal (Proxy :: Proxy quote) :: Text

abVenue
    :: forall venue.
       AnyBook venue
    -> Text
abVenue (AnyBook ob) =
    toS $ symbolVal (Proxy :: Proxy venue) :: Text

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











{-

marketList
   :: forall venue m.
      (EnumMarkets venue, MonadIO m)
   => Proxy venue
   -> AppM m [Market venue]

-}



-- ^ Return a list of shortest path *edges*
--  given a list of shortest path *nodes*
-- shortestPathEdges
--     :: (G.Graph gr, Real b)
--     => gr a b
--     -> G.LPath a                    -- Nodes defining a shortest path
--     -> Maybe (Text, [G.LEdge b])    -- 'Nothing' if no such path
-- shortestPathEdges g [node]   = Just (snd node, [])
-- shortestPathEdges g (node:nextNode:path) = do
--     let edgeLst = G.out g node
--         toNode (_, to, _) = to
--         toEdges = filter (\edge -> toNode edge == fst nextNode) edgeLst
--     shortestEdge <- head (sortOn G.edgeLabel toEdges)
--     (label, remainingEdges) <- shortestPathEdges g (nextNode:path)
--     return $ (label, shortestEdge : remainingEdges)

-- main :: (Par.MonadParallel m, MonadIO m) => AppM m ()
-- main = do
--     marketLst <- allMarkets
--     let getQuote (AnyMarket mkt) = miQuote mkt
--     print $ nub $ map getQuote marketLst



-- main :: (Par.MonadParallel m, MonadIO m) => AppM m ()
-- main = do
--    edges <- allEdges
--    putStrLn $ "Edges: " ++ show (length edges)
--    putStrLn $ "Nodes: " ++ show (length $ getNodes edges)
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






-- | Same as 'G.sp' but return a labeled path, 
--  as opposed to an unlabeled path
-- spL :: (G.Graph gr, Real b)
--     => G.Node -- ^ Start
--     -> G.Node -- ^ Destination
--     -> gr a b
--     -> Maybe (G.LPath a)
-- spL s t g = case getLPath t (spTree s g) of
--     [] -> Nothing
--     p  -> Just p



-- main' :: AppM IO ()
-- main' = do
--     let g = G.empty
--         v = zip [0..23] [()..]
--         e = [[ (1,20,()),(20,1,())     ]
--             ,[ (1,20,()),(20,1,())     ]
--             ,[ (10,20,()),(20,10,())   ]
--             ,[ (10,1,()),(1,10,())     ]
--             ,[ (4,20,()),(20,4,())     ]
--             ,[ (4,1,()),(1,4,())       ]
--             ,[ (23,1,()),(1,23,())     ]
--             ,[ (23,20,()),(20,23,())   ]
--             ,[ (22,20,()),(20,22,())   ]
--             ,[ (22,1,()),(1,22,())     ]
--             ,[ (21,20,()),(20,21,())   ]
--             ,[ (1,20,()),(20,1,())     ]
--             ,[ (10,1,()),(1,10,())     ]
--             ,[ (19,1,()),(1,19,())     ]
--             ,[ (18,1,()),(1,18,())     ]
--             ,[ (17,1,()),(1,17,())     ]
--             ,[ (16,1,()),(1,16,())     ]
--             ,[ (15,1,()),(1,15,())     ]
--             ,[ (14,1,()),(1,14,())     ]
--             ,[ (13,1,()),(1,13,())     ]
--             ,[ (12,1,()),(1,12,())     ]
--             ,[ (11,1,()),(1,11,())     ]
--             ,[ (4,1,()),(1,4,())       ]
--             ,[ (10,1,()),(1,10,())     ]
--             ,[ (9,1,()),(1,9,())      ]
--             ,[ (8,1,()),(1,8,())      ]
--             ,[ (7,4,()),(4,7,())      ]
--             ,[ (6,4,()),(4,6,())      ]
--             ,[ (5,4,()),(4,5,())      ]
--             ,[ (3,4,()),(4,3,())      ]
--             ,[ (2,1,()),(1,2,())      ]
--             ,[ (0,1,()),(1,0,())      ]
--             ]
--         g2 = foldr G.insNode g v
--         g3 = foldr G.insEdges g2 e
--     print (g3 :: G.Gr () ())
--     print (allPaths 20 g3)

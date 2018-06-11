{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}

module Paths where

import CPrelude
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
import qualified Data.Graph.Inductive.Internal.RootPath as G
import qualified Control.Monad.Parallel   as Par
import Data.List (nub, tail, last)
import qualified Data.Text as T
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.HashMap.Strict as Map
import qualified Money
import qualified Data.Vector  as Vec


slippagePercent :: Rational
slippagePercent = 5 % 1

-- | Tag some value onto some other value
data Pair v t = Pair { pFst :: v, pSnd :: t }
-- instance Real v => Real (Pair v t)

data AnyMarketList =
    forall venue.
    MarketBook venue
        => AnyMarketList [Market venue]

-- | Just some order book
data ABook =
    forall venue.
    MarketBook venue
       => ABook { aBook :: AnyBook venue }

-- | Some USD-exchange rate
data USDRate =
    forall base.
    KnownSymbol base
        => USDRate { usdRate :: Money.ExchangeRate base "USD" }

type USDRateMap = Map.HashMap Text Rational

getRate :: USDRateMap -> Text -> Maybe Rational
getRate = flip Map.lookup

toRate
    :: G.Graph gr
    => gr Text (Pair Rational ABook)
    -> [G.LNode (Pair Rational ABook)]
    -> Maybe (Text, Rational)
toRate _ [] = Nothing
toRate g nodes = Just
    (lastNodeLabel, rate)
  where
    rate = foldr (*) (1%1) $ map (pFst . snd) nodes
    lastNode = fst $ last nodes
    lastNodeLabel = fromMaybe (error $ "toRate: no such node in graph: " ++ show lastNode) $
        G.lab g lastNode


buildRateMap :: [ABook] -> USDRateMap
buildRateMap books =
    toRateMap nodeMap (graph :: G.Gr Text (Pair Rational ABook))
  where
    (graph, nodeMap) = buildGraph toRateEdges books


toRateMap
    :: G.Graph gr
    => NodeMap
    -> gr Text (Pair Rational ABook)
    -> USDRateMap
toRateMap nodeMap g =
      foldr insertRate Map.empty
    $ catMaybes
    $ map (toRate g)
    $ allPaths (symNode "USD") g
  where
    insertRate (sym, rate) rateMap = Map.insert sym rate rateMap
    symNode sym = fromMaybe (error $ show sym ++  " not found in map") $
        Map.lookup sym nodeMap


type NodeMap = Map.HashMap Text Int
type GraphM m a = S.StateT NodeMap m a

buildGraph
    :: forall gr edgeLabel. G.DynGraph gr
    => (NodeMap -> ABook -> [G.LEdge edgeLabel])
    -> [ABook]
    -> (gr Text edgeLabel, NodeMap)
buildGraph toEdges books =
    S.runState (buildGraphM toEdges books) Map.empty

buildGraphM
    :: forall gr m edgeLabel. (Monad m, G.DynGraph gr)
    => (NodeMap -> ABook -> [G.LEdge edgeLabel])
    -> [ABook]
    -> GraphM m (gr Text edgeLabel)
buildGraphM toEdges =
    foldrM insertBook G.empty
  where
    symbolNode :: Text -> NodeMap -> (G.LNode Text, NodeMap)
    symbolNode symStr bimap =
            let nextNode = Map.size bimap in
            case Map.lookup symStr bimap of
                Nothing   -> ((nextNode, symStr), Map.insert symStr nextNode bimap)
                Just node -> ((node,     symStr), bimap)
    insertBook
        :: ABook
        -> gr Text edgeLabel
        -> GraphM m (gr Text edgeLabel)
    insertBook ab@(ABook anyBook) g = do
        bimap <- S.get
        let (baseSym, quoteSym) = traceIt $ (abBase anyBook, abQuote anyBook)
            (baseNode,  baseBimap)  = symbolNode baseSym bimap
            (quoteNode, quoteBimap) = symbolNode quoteSym baseBimap
        S.put quoteBimap
        let graphBaseNode  = G.insNode baseNode  g
            graphQuoteNode = G.insNode quoteNode graphBaseNode
        return $ G.insEdges (toEdges quoteBimap ab) graphQuoteNode

toRateEdges
    :: NodeMap
    -> ABook
    -> [G.LEdge (Pair Rational ABook)]
toRateEdges symbolMap ab@(ABook anyBook@(AnyBook ob)) =
   catMaybes
       [ mkSellEdge <$> bestBidM
       , mkBuyEdge  <$> bestAskM
       ]
 where
   bestBidM = rationalPrice <$> obBids ob Vec.!? 0
   bestAskM = rationalPrice <$> obAsks ob Vec.!? 0
   mkSellEdge bestBid = (baseNode, quoteNode, mkPair bestBid)
   mkBuyEdge  bestAsk = (quoteNode, baseNode, mkPair bestAsk)
   mkPair a = Pair a ab
   -- Util
   rationalPrice = Money.fromExchangeRate . oPrice
   baseNode = lookupOrFail (abBase anyBook)
   quoteNode = lookupOrFail (abQuote anyBook)
   lookupOrFail sym = fromMaybe (error $ "toEdges: symbol not found: " ++ show sym) $
        Map.lookup sym symbolMap

-- toDepthEdges
--     :: NodeMap
--     -> ABook
--     -> [G.LEdge (Pair Rational ABook)]
-- toDepthEdges symbolMap ab@(ABook anyBook@(AnyBook ob)) =
--    [ (baseNode, quoteNode, pairSell)
--    , (quoteNode, baseNode, pairBuy)
--    ]
--  where
--    baseNode = lookupOrFail (abBase anyBook)
--    quoteNode = lookupOrFail (abQuote anyBook)
--    pairSell = Pair (Match.resQuoteQty $ Match.slippageSell ob slippagePercent) ab
--    pairBuy  = Pair (Match.resQuoteQty $ Match.slippageBuy ob slippagePercent) ab
--    slippage = fromMaybe infinity . Match.slippage
--    lookupOrFail sym = fromMaybe (error $ "toEdges: symbol not found: " ++ show sym) $
--         Map.lookup symbolMap sym

traceIt a = show a `trace` a

abBase
    :: AnyBook venue
    -> Text
abBase (AnyBook ob) =
    case ob of
        (ob :: OrderBook base quote venue) ->
            toS $ symbolVal (Proxy :: Proxy base) :: Text

abQuote
    :: AnyBook venue
    -> Text
abQuote (AnyBook ob) =
    case ob of
        (ob :: OrderBook base quote venue) ->
            toS $ symbolVal (Proxy :: Proxy quote) :: Text


allPaths
    :: G.Graph gr
    => G.Node       -- ^ Start node
    -> gr a b
    -> [[G.LNode b]]
allPaths start g =
      map (tail . G.unLPath)
    $ G.lbft start g
    -- From source of Data.Graph.Inductive.Query.BFS:
    -- -- Note that the label of the first node in a returned path is meaningless;
    -- -- all other nodes are paired with the label of their incoming edge.


main = do
    books <- allBooks
    print $ buildRateMap books
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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
module Paths.Types where


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
import qualified Data.Graph.Inductive.Query.MST as G
import qualified Data.Graph.Inductive.Internal.RootPath as G
import qualified Control.Monad.Parallel   as Par
import Data.List (nub, tail, last, init)
import qualified Data.Text as T
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.HashMap.Strict as Map
import qualified Money
import qualified Data.Vector  as Vec


data Pair v t = Pair { pFst :: v, pSnd :: t }
    deriving Eq

instance Show Edge where
    show (Pair book rat) =
        printf "<%s %f>" (show book) (realToFrac rat :: Double)

instance Functor (Pair (Maybe v)) where
    fmap f (Pair v t) = Pair v (f t)

instance (Eq v, Real t) => Real (Pair (Maybe v) t) where
    toRational = toRational . pSnd

instance Num t => Num (Pair (Maybe v) t) where
    Pair v1 t1 + Pair _ t2 = Pair v1 (t1+t2)
    Pair v1 t1 * Pair _ t2 = Pair v1 (t1*t2)
    abs = fmap abs
    signum = fmap signum
    fromInteger i = Pair Nothing (fromInteger i)
    negate = fmap negate

instance (Eq v, Ord t) => Ord (Pair (Maybe v) t) where
    (Pair _ t1) <= (Pair _ t2) = t1 <= t2

data AnyMarketList =
    forall venue.
    MarketBook venue
        => AnyMarketList [Market venue]

-- | Just some order book
data ABook =
    forall venue.
    MarketBook venue
       => ABook { aBook :: AnyBook venue }

instance Show ABook where
    show (ABook ab) =
        toS $ abBase ab <> "/" <> abQuote ab <> " (" <> abVenue ab <> ")"

instance Eq ABook where
    _ == _ = True

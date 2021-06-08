{-# OPTIONS_GHC -fno-warn-orphans #-}
module CryptoVenues.Internal.Prelude
( module Protolude
, module Error
, module TypeLits
, module Prelude
, module EitherT
, sameSym
, trace
, Vector
, fmapL
, printf
, fail
, S.BaseUrl(..), S.Scheme(..)
, show'
, groupOnOrd
)
where

import Protolude hiding (trace, Show, show)
import Prelude (String, Show, show, id, mod, lookup, error)
import CryptoVenues.Types.Error as Error
import Debug.Trace (trace)
import GHC.TypeLits as TypeLits (Symbol, KnownSymbol, SomeSymbol(..)
                                , sameSymbol, symbolVal, someSymbolVal
                                )
import Control.Monad.Trans.Except as EitherT
import Control.Monad.Fail
import           Data.Vector  (Vector)
import Text.Printf
import Data.EitherR (fmapL)
import qualified Servant.Client.Core.Reexport as S

show' :: Show a => a -> Text
show' = toS . show

sameSym :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Bool
sameSym a b = isJust (sameSymbol a b)

instance MonadFail (Either Text) where
  fail = Left . toS

-- | @groupOnOrd f lst@ first sorts the list by @f@ and then groups by @f@.
--
-- Example:
--
-- >>> groupOnOrd fst [("a", 1), ("b", 2), ("a", 3)]
-- [[("a",1),("a",3)],[("b",2)]]
groupOnOrd :: Ord o => (t -> o) -> [t] -> [[t]]
groupOnOrd f =
  groupBy (\m1 m2 -> f m1 == f m2) . sortOn f

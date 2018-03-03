{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CPrelude
( module Protolude
, module Safe
, module Error
, module TypeLits
, module Prelude
, module EitherT
, module AppM
, sameSym
, trace
, Vector
, fmapL
, printf
, fail
)
where

import Protolude hiding (trace, Show, show, error)
import Prelude (String, Show, show, id, mod, lookup, error)
import Types.Error as Error
import Types.AppM as AppM
import Debug.Trace (trace)
import Safe
import GHC.TypeLits as TypeLits (Symbol, KnownSymbol, SomeSymbol(..)
                                , sameSymbol, symbolVal, someSymbolVal
                                )
import Control.Monad.Trans.Except as EitherT
import Control.Monad.Fail
import Control.Monad.Except hiding (fail)
import           Data.Vector  (Vector)
import Text.Printf
import Data.EitherR (fmapL)
import Control.Monad.Trans.Reader
import qualified Network.HTTP.Client   as HTTP

import qualified Control.Monad.Parallel   as Par


sameSym :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Bool
sameSym a b = isJust (sameSymbol a b)

instance MonadFail (Either Text) where
  fail = Left . toS

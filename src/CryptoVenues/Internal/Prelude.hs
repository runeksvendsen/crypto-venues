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

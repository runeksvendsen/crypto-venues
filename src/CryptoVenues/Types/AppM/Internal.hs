{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CryptoVenues.Types.AppM.Internal
( AppM
, Config(..)
, runAppM
, throwLeft
)
where

import Prelude
import Control.Monad.Reader
import CryptoVenues.Types.Error as Error
import Control.Monad.Except
import qualified Network.HTTP.Client      as HTTP
import qualified Control.Monad.Parallel   as Par
import qualified CryptoVenues.Internal.RateLimitCache   as Cache


throwLeft :: MonadError e m => Either e a -> m a
throwLeft = either throwError return

data Config
   = Config
   { cfgMan             :: HTTP.Manager
   , cfgNumMaxRetries   :: Word
   , cfgRateLimitCache  :: Cache.RateLimitCache
   }

newtype AppM m a = AppM { getAppM :: ReaderT Config (ExceptT Error m) a }
   deriving
   ( Applicative
   , Functor
   , Monad
   , MonadIO
   , MonadError Error
   , MonadReader Config
   , Par.MonadParallel
   )

instance MonadTrans AppM where
    lift = AppM . lift . lift

runAppM :: MonadIO m => Config -> AppM m a -> m (Either Error a)
runAppM cfg appM =
    runExceptT $ runReaderT (getAppM appM) cfg

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CryptoVenues.Types.AppM where

import Prelude
import Control.Monad.Reader
import CryptoVenues.Types.Error as Error
import Control.Monad.Except
import qualified Network.HTTP.Client      as HTTP
import qualified Control.Monad.Parallel   as Par


throwLeft :: MonadError e m => Either e a -> m a
throwLeft = either throwError return

data Config
   = Config
   { cfgMan         :: HTTP.Manager
   , numMaxRetries  :: Word
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

runAppM :: MonadIO m => HTTP.Manager -> Word -> AppM m a -> m (Either Error a)
runAppM man numMaxRetries appM = runExceptT $ runReaderT (getAppM appM) cfg
   where cfg = Config man numMaxRetries

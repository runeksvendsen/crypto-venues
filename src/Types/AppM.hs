{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types.AppM where

import Prelude
import Data.Proxy
import Control.Monad.Reader
import Types.Error as Error
import Control.Monad.Except
import qualified Network.HTTP.Client   as HTTP
import qualified Control.Monad.Parallel   as Par


throwLeft :: MonadError e m => Either e a -> m a
throwLeft = either throwError return

newtype AppM m a = AppM { getAppM :: ReaderT HTTP.Manager (ExceptT FetchErr m) a }
   deriving ( Applicative, Functor, Monad, MonadIO
            , MonadError FetchErr, MonadReader HTTP.Manager
            , Par.MonadParallel)

runAppM :: HTTP.Manager -> AppM IO a -> IO (Either FetchErr a)
runAppM man appM = runExceptT $ runReaderT (getAppM appM) man


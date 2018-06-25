{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CryptoVenues.Types.AppM where

import Prelude
import Control.Monad.Reader
import CryptoVenues.Types.Error as Error
import Control.Monad.Except
import qualified Network.HTTP.Client      as HTTP
import qualified Control.Monad.Parallel   as Par
--import qualified Control.Monad.Logger     as Log


throwLeft :: MonadError e m => Either e a -> m a
throwLeft = either throwError return

data Config
   = Config
   { cfgMan    :: HTTP.Manager
   -- , cfgLogLvl :: Log.LogLevel
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
   -- , Log.MonadLogger
   -- , MonadTrans
   )

--instance (MonadIO m, Par.MonadParallel m) => Par.MonadParallel (AppM m) where
--   bindM2 f ma mb = do
--      man <- asks cfgMan
--      either throwError return =<< Par.bindM2 (f' man) (runAppM man ma) (runAppM man mb)
--     where f' man (Right a) (Right b) = runAppM man (f a b)
--           f' _   (Left e)  _         = return (Left e)
--           f' _   _         (Left e)  = return (Left e)

runAppM :: MonadIO m => HTTP.Manager -> AppM m a -> m (Either Error a)
runAppM man appM = runExceptT $ runReaderT (getAppM appM) cfg
   where cfg = Config man


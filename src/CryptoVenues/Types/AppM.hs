module CryptoVenues.Types.AppM
( Internal.AppM
, Internal.Config
, runAppM
, Internal.throwLeft
)
where

import qualified CryptoVenues.Types.AppM.Internal as Internal

import Prelude
import Control.Monad.Reader
import CryptoVenues.Types.Error as Error
import qualified Network.HTTP.Client      as HTTP
import qualified CryptoVenues.Internal.RateLimitCache   as Cache


runAppM :: MonadIO m => HTTP.Manager -> Word -> Internal.AppM m a -> m (Either Error a)
runAppM man numMaxRetries appM = do
    cache <- Cache.create
    Internal.runAppM (mkCfg cache) appM
  where
    mkCfg = Internal.Config man numMaxRetries

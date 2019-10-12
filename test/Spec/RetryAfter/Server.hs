module Spec.RetryAfter.Server
( retryAfter
, Config(..)
)
where

import           Prelude
import           Protolude.Conv             (toS)
import           Servant.API
import qualified Servant
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp
import qualified Servant.Client             as SC
import qualified Servant.Client.Core        as SCC


retryAfter
    :: Config
    -> ( IO ()
       , Word -> Word -> SC.ClientM ()
       , SCC.BaseUrl
       )
retryAfter cfg =
  ( retryAfterServer cfg
  , SC.client api
  , baseUrl
  )
  where
    baseUrl = SCC.BaseUrl
      { baseUrlScheme = SCC.Http
      , baseUrlHost = "localhost"
      , baseUrlPort = fromIntegral $ cfgPort cfg
      , baseUrlPath = ""
      }

retryAfterServer :: Config -> IO ()
retryAfterServer cfg =
    Warp.run
      (fromIntegral $ cfgPort cfg)
      retryAfterApp

retryAfterApp :: Wai.Application
retryAfterApp =
    Servant.serve api handler
  where
    handler statusCode waitSeconds =
      Servant.throwError (statusError statusCode waitSeconds)

api :: Servant.Proxy RetryAfter
api = Servant.Proxy

statusError :: Word -> Word -> Servant.ServerError
statusError statusCode waitSeconds = Servant.ServerError
    { errHTTPCode = fromIntegral statusCode
    , errReasonPhrase = "Too many requests"
    , errBody = ""
    , errHeaders = [("Retry-After", toS $ show waitSeconds)]
    }

type RetryAfter
   =  "retry_after"
   :> Capture "status_code" Word
   :> Capture "wait_seconds" Word
   :> Get '[JSON] ()

data Config = Config
    { cfgPort :: Word
    }

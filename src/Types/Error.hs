module Types.Error where

import Prelude
import Control.DeepSeq
import GHC.Generics
import Text.Printf
import Control.Exception
import Servant.Common.Req
import Servant.Client                        as SC
import qualified Network.HTTP.Types.Status   as Status


-- |
data FetchErr
   = TooManyRequests             -- ^ We should slow down
   | ConnectionErr SomeException -- ^ Something's wrong with the connection
   | EndpointErr UrlReq          -- ^ The endpoint has fucked up
   | InternalErr String          -- ^ We've fucked up
      deriving (Show, Generic)  -- TODO: Proper Show instance

-- Let's be conservative to begin with
shouldRetry :: FetchErr -> Bool
shouldRetry TooManyRequests = True
shouldRetry _ = False

fromServant :: ServantError -> FetchErr
fromServant FailureResponse{..} =
   handleStatusCode (Status.statusCode responseStatus) failingRequest
fromServant (ConnectionError someEx) =
   ConnectionErr someEx
fromServant DecodeFailure{..} =
   InternalErr $ "Decode error: " ++ decodeError
fromServant UnsupportedContentType{..} =
   InternalErr $ "Unsupported content type: " ++ show responseContentType
fromServant InvalidContentTypeHeader{..} =
   InternalErr $ "Invalid content type header: " ++ show responseContentTypeHeader

handleStatusCode :: Int -> UrlReq -> FetchErr
handleStatusCode 429 _ = TooManyRequests
handleStatusCode code req
   | code >= 500 && code < 600 = EndpointErr req
   | otherwise = InternalErr $
      printf "Unhandled failure response. Code: %d. Req: %s" code (show req)


{-# LANGUAGE ExistentialQuantification #-}
module Types.Error
( Error(..)
, ErrType(..)
, FetchErr(..)
, VenueFetchErr(..)
, IsError(..)
, shouldRetry
, fromServant
)
where

import Prelude
import Types.Market
import Data.Proxy
import Control.DeepSeq
import GHC.Generics
import GHC.TypeLits
import Text.Printf
import Control.Exception
import Servant.Common.Req
import Servant.Client                        as SC
import qualified Network.HTTP.Types.Status   as Status


data Error = forall venue. KnownSymbol venue =>
   Error
   { eContext  :: ErrType venue
   , eFetchErr :: FetchErr
   }

instance Show Error where
   show (Error ctx fe) =
      printf "(%s) %s" (show ctx) (show fe)

data ErrType venue
   = VenueEnumErr (Proxy venue)
   | VenueLimitErr (Proxy venue)
   | BookErr (Market venue)

instance KnownSymbol venue => Show (ErrType venue) where
   show (VenueEnumErr p) = symbolVal p ++ " EnumMarkets"
   show (VenueLimitErr p) = symbolVal p ++ " rateLimit"
   show (BookErr mkt) = show mkt

-- |
data FetchErr
   = TooManyRequests             -- ^ We should slow down
   | ConnectionErr SomeException -- ^ Something's wrong with the connection
   | EndpointErr UrlReq          -- ^ The endpoint has fucked up
   | InternalErr String          -- ^ We've fucked up
      deriving (Show, Generic)  -- TODO: Proper Show instance

data VenueFetchErr
   = forall venue.
     KnownSymbol venue
   => VenueFetchErr (Proxy venue) FetchErr

class IsError err info where
   fromFetchErr :: info -> FetchErr -> err

instance KnownSymbol venue => IsError VenueFetchErr (Proxy venue) where
   fromFetchErr = VenueFetchErr

shouldRetry :: Error -> Bool
shouldRetry Error{..} = shouldRetryFE eFetchErr

-- | Should we retry a failed request?
shouldRetryFE :: FetchErr -> Bool
shouldRetryFE TooManyRequests = True
shouldRetryFE _               = False -- Let's be conservative to begin with

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


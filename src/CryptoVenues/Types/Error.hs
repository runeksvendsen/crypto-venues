{-# LANGUAGE ExistentialQuantification #-}
module CryptoVenues.Types.Error
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
import Protolude.Conv (toS)
import CryptoVenues.Types.Market
import Data.Proxy
import Control.DeepSeq
import GHC.Generics
import GHC.TypeLits
import Text.Printf
import Control.Exception
-- import Servant.Common.Req
import Servant.Client.Core
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
   = TooManyRequests            -- ^ We should slow down
   | ConnectionErr String       -- ^ Something's wrong with the connection
   | EndpointErr BaseUrl        -- ^ The endpoint messed up
   | InternalErr String         -- ^ We messed up
      deriving (Show, Generic)  -- TODO: Proper Show instance

-- instance Show FetchErr where
--     show (EndpointErr url) = "EndpointErr: " ++ show (showBaseUrl url)

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
shouldRetryFE (InternalErr _) = False
shouldRetryFE _ = True

fromServant :: BaseUrl -> ServantError -> FetchErr
fromServant url (FailureResponse res) =
   handleStatusCode res url
fromServant _ (ConnectionError errText) =
   ConnectionErr (show errText)
fromServant _ (DecodeFailure decodeError _) =
   InternalErr $ "Decode error: " ++ show decodeError
fromServant _ (UnsupportedContentType mediaType _) =
   InternalErr $ "Unsupported content type: " ++ show mediaType
fromServant _ (InvalidContentTypeHeader res) =
   InternalErr $ "Invalid content type header. Response: " ++ show res

handleStatusCode :: Response -> BaseUrl -> FetchErr
handleStatusCode res url
    | statusCode == 429 = TooManyRequests
    | statusCode >= 500 && statusCode < 600 = EndpointErr url
    | otherwise = InternalErr $
        printf "Unhandled failure response. Code: %d. Msg: %s. Url: %s. Body:\n%s"
               statusCode
               (show (toS $ Status.statusMessage status :: String))
               (show $ SC.showBaseUrl url)
               (toS $ responseBody res :: String)
  where
    status = responseStatusCode res
    statusCode = Status.statusCode status


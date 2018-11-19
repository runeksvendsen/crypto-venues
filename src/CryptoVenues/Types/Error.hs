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
import Formatting
import Protolude.Conv (toS)
import CryptoVenues.Types.Market
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Text.Printf
-- import Servant.Common.Req
import Servant.Client.Core
import Servant.Client                        as SC
import qualified Network.HTTP.Types.Status   as Status
import qualified Data.Text as T


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
   | InternalErr T.Text         -- ^ We messed up
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
fromServant _ (DecodeFailure decodeError res) =
   InternalErr $ T.unwords
              [ "Decode Error:"
              , toS decodeError
              , "Response body:"
              , toS $ responseBody res
              ]
fromServant _ (UnsupportedContentType mediaType _) =
   InternalErr . toS $ format ("Unsupported content type: " % string) (show mediaType)
fromServant _ (InvalidContentTypeHeader res) =
   InternalErr . toS $ format ("Invalid content type header. Response: " % string) (show res)

handleStatusCode :: Response -> BaseUrl -> FetchErr
handleStatusCode res url
    | statusCode == 429 = TooManyRequests
    | statusCode >= 500 && statusCode < 600 = EndpointErr url
    | otherwise = InternalErr failureResponseText
  where
    status = responseStatusCode res
    statusCode = Status.statusCode status
    failureResponseText :: T.Text
    failureResponseText = toS $
        format ("Unhandled failure response.\nCode: " % int %
                "\nMsg: " % text %
                "\nUrl: " % string %
                "\nBody: " % text)
            statusCode
            (toS $ Status.statusMessage status)
            (show $ SC.showBaseUrl url)
            (toS $ responseBody res)



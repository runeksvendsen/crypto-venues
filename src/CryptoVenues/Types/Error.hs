{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NumDecimals #-}
module CryptoVenues.Types.Error
( Error(..)
, ErrType(..)
, FetchErr(..)
, VenueFetchErr(..)
, IsError(..)
, toRetryAction
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
import Data.Maybe                            (listToMaybe)
import Data.Foldable                         (toList)
import Text.Read                             (readMaybe)
import Control.Monad                         ((<=<))
import Servant.Client.Core
import Servant.Client                        as SC
import qualified Network.HTTP.Types.Status   as Status
import qualified Data.Text as T
import qualified Data.Time.Units             as Time
import qualified Data.Sequence               as Seq
import qualified Network.HTTP.Types.Header   as Header
import qualified Control.Retry               as Re
import Debug.Trace

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
   = TooManyRequests (Maybe Time.Second)  -- ^ We should slow down
                                          --  (Maybe the time we should wait before next request)
   | ConnectionErr String       -- ^ Something's wrong with the connection
   | EndpointErr BaseUrl        -- ^ The endpoint messed up
   | InternalErr T.Text         -- ^ We messed up
      deriving (Eq, Show, Generic)  -- TODO: Proper Show instance

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

toRetryAction :: Error -> Re.RetryAction
toRetryAction Error{..} =
   toRetryActionFE eFetchErr

toRetryActionFE :: FetchErr -> Re.RetryAction
toRetryActionFE (InternalErr _)   = Re.DontRetry
toRetryActionFE (EndpointErr _)   = Re.ConsultPolicy
toRetryActionFE (ConnectionErr _) = Re.ConsultPolicy
toRetryActionFE (TooManyRequests timeM) =
   maybe Re.ConsultPolicy overrideDelay timeM
  where
   overrideDelay :: Time.TimeUnit t => t -> Re.RetryAction
   overrideDelay = Re.ConsultPolicyOverrideDelay . fromIntegral . Time.toMicroseconds

fromServant :: BaseUrl -> ClientError -> FetchErr
fromServant url (FailureResponse _ res) =
   show res `trace` handleStatusCode res url
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
    | statusCode == 429 ||
      -- "binance" returns 418 when the client has been banned.
      -- interpret this as "TooManyRequests"
      statusCode == 418 = TooManyRequests (headerRetryAfter res)
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

-- Get the number of seconds to wait before retrying from
--  a "Retry-After" header, if present. Assumed to contain
--  waiting time in seconds.
headerRetryAfter
   :: Response
   -> Maybe Time.Second
headerRetryAfter res =
    fmap fromSeconds . readMaybe <=< fmap toS . listToMaybe . map snd . toList $
        Seq.filter ((== Header.hRetryAfter) . fst) (responseHeaders res)
  where
    fromSeconds :: Integer -> Time.Second
    fromSeconds = Time.fromMicroseconds . (* 1e6)

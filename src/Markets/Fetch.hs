module Markets.Fetch
( marketList
, module Venue.Types
)
where

import CPrelude
import Markets.Types
import Venue.Types
import Fetch
import qualified Network.HTTP.Client   as HTTP
import qualified Servant.Common.Req    as Req
import Data.Either


marketList
   :: HTTP.Manager
   -> AnyVenue
   -> IO (Either Req.ServantError [AnyMarket])
marketList man (AnyVenue p) = case p of
   (Proxy :: Proxy venue) -> runEitherT $ do
      res :: MarketList venue <- hoistEither =<< fetch man
      return $ map AnyMarket (getMarkets res)

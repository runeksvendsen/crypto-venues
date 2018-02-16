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
--import Control.Monad.Trans.Except

marketList
   :: MonadIO m
   => HTTP.Manager
   -> AnyVenue
   -> ExceptT Req.ServantError m [AnyMarket] -- IO (Either Req.ServantError [AnyMarket])
marketList man (AnyVenue p) = case p of
   (Proxy :: Proxy venue) -> do
      res :: MarketList venue <- throwErr =<< fetch man
      return $ map AnyMarket (getMarkets res)

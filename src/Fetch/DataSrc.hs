{-# LANGUAGE TypeOperators #-}
module Fetch.DataSrc where

import CPrelude
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import           Servant.API
import qualified Data.Aeson            as Json
import qualified Network.HTTP.Client   as HTTP


class DataSource dataType where
   dataSrc :: DataSrc dataType

data DataSrc dataType = DataSrc
   { dsUrl     :: S.BaseUrl
   , dsClientM :: SC.ClientM dataType
   }

srcFetch
   :: forall m dataType.
      MonadIO m
   => HTTP.Manager
   -> DataSrc dataType
   -> m (Either SC.ServantError dataType)
srcFetch man ds = liftIO $ SC.runClientM clientM env
   where env = SC.ClientEnv man (dsUrl ds)
         clientM = dsClientM ds

fetch :: forall m dataType.
         (MonadIO m, DataSource dataType)
      => HTTP.Manager
      -> m (Either SC.ServantError dataType)
fetch man = srcFetch man (dataSrc :: DataSrc dataType)



{-# LANGUAGE TypeOperators #-}
module Fetch.DataSrc where

import CPrelude
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import           Servant.API
import qualified Data.Aeson            as Json
import qualified Network.HTTP.Client   as HTTP


data DataSrc dataType = DataSrc
   { dsUrl     :: S.BaseUrl
   , dsClientM :: SC.ClientM dataType
   }

srcFetch
   :: forall m dataType.
      MonadIO m
   => HTTP.Manager
   -> DataSrc dataType
   -> m (Either FetchErr dataType)
srcFetch man ds = liftIO cmRes
   where cmRes = fmapL fromServant <$> SC.runClientM (dsClientM ds) env
         env = SC.ClientEnv man (dsUrl ds)


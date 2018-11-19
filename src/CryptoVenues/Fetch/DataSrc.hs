{-# LANGUAGE TypeOperators #-}
module CryptoVenues.Fetch.DataSrc where

import CryptoVenues.Internal.Prelude
import qualified Servant.Client        as SC
import qualified Network.HTTP.Client   as HTTP


data DataSrc dataType = DataSrc
   { dsUrl     :: BaseUrl
   , dsClientM :: SC.ClientM dataType
   }

srcFetch
   :: forall m dataType.
      MonadIO m
   => HTTP.Manager
   -> DataSrc dataType
   -> (SC.ServantError -> m SC.ServantError)
   -> m (Either FetchErr dataType)
srcFetch man ds modifyErr = do
    resE <- liftIO $ SC.runClientM (dsClientM ds) env
    resModE <- case resE of
        Left err  -> Left <$> modifyErr err
        resR      -> return resR
    return $ fmapL (fromServant url) resModE
  where
     env = SC.ClientEnv man url Nothing
     url = dsUrl ds

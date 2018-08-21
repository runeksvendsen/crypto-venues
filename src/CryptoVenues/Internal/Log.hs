module CryptoVenues.Internal.Log
( module Logging
, infoS
, info
, lerrorS
)
where

import Protolude
import Control.Logging as Logging


infoS :: Text -> Text -> IO ()
infoS = loggingLogger LevelInfo

info :: Text -> IO ()
info = infoS ""

lerrorS :: Text -> Text -> IO ()
lerrorS = loggingLogger LevelError

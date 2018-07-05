module CryptoVenues.Internal.Log
( module Logging
, infoS
, info
, lerrorS
)
where

import CryptoVenues.Internal.CPrelude
import Control.Logging as Logging
--import qualified Control.Logging    as Log


infoS :: Text -> Text -> IO ()
infoS = loggingLogger LevelInfo

info :: Text -> IO ()
info = infoS ""

lerrorS :: Text -> Text -> IO ()
lerrorS = loggingLogger LevelError
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Venue.Types where

import CPrelude
import Markets.Types
import Fetch.DataSrc
import Fetch.MarketBook
import qualified Data.Aeson            as Json


data AnyVenue
   = forall venue.
   ( MarketBook venue
   , DataSource (MarketList venue)
   )
   => AnyVenue (Proxy venue)

instance Show AnyVenue where
   show (AnyVenue p) = symbolVal p

instance Json.ToJSON AnyVenue where
   toJSON (AnyVenue p) = Json.toJSON (toS $ symbolVal p :: Text)

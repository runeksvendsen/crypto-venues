{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Venue.Types where

import CPrelude
import Markets.Types
import Fetch
import qualified Data.Aeson            as Json


data AnyVenue
   = forall venue.
   ( MarketBook venue
   , DataSource (MarketList venue)
   )
   => AnyVenue (Proxy venue)

instance Json.ToJSON AnyVenue where
   toJSON (AnyVenue p) = Json.toJSON (toS $ symbolVal p :: Text)

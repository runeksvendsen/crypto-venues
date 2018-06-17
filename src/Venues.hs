module Venues
( allVenues
, venueLookup
, AnyVenue(..)
)
where

import CPrelude
import Fetch.MarketBook

import Venues.GDAXl2       as GDAXl2         ()
import Venues.GDAXl3       as GDAXl3         ()
import Venues.Bitstamp     as Bitstamp       ()
import Venues.Bitfinex     as Bitfinex       ()
import Venues.BitfinexV2   as BitfinexV2     ()
import Venues.Bittrex      as Bittrex        ()
import Venues.Binance      as Binance        ()

import qualified Network.HTTP.Client   as HTTP
import qualified Servant.Client        as SC
import qualified Data.HashMap.Strict   as HM


-- | Canonical list of all supported venues
allVenues :: [AnyVenue]
allVenues =
   [ AnyVenue (Proxy :: Proxy "bitfinex")
   , AnyVenue (Proxy :: Proxy "bittrex")
   , AnyVenue (Proxy :: Proxy "binance")
--    , AnyVenue (Proxy :: Proxy "bitstamp")
   ]

-- | Map of all supported venue names to its corresponding 'AnyVenue'
venueMap :: HM.HashMap Text AnyVenue
venueMap = HM.fromList $ map (\v@(AnyVenue p) -> (toS $ symbolVal p, v)) allVenues

-- | Lookup by 'Text' string in the map of all venues
venueLookup :: Text -> Maybe AnyVenue
venueLookup = (`HM.lookup` venueMap)



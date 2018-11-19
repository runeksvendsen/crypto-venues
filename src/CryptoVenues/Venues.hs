module CryptoVenues.Venues
( allVenues
, venueLookup
, AnyVenue(..)
)
where

import CryptoVenues.Internal.CPrelude
import CryptoVenues.Fetch.MarketBook

import CryptoVenues.Venues.Coinbase     as Coinbase       ()
import CryptoVenues.Venues.Bitstamp     as Bitstamp       ()
import CryptoVenues.Venues.Bitfinex     as Bitfinex       ()
import CryptoVenues.Venues.Bittrex      as Bittrex        ()
import CryptoVenues.Venues.Binance      as Binance        ()

import qualified Data.HashMap.Strict   as HM


-- | Canonical list of all supported venues
allVenues :: [AnyVenue]
allVenues =
   [ AnyVenue (Proxy :: Proxy "bitfinex")
   , AnyVenue (Proxy :: Proxy "bittrex")
   , AnyVenue (Proxy :: Proxy "binance")
   , AnyVenue (Proxy :: Proxy "bitstamp")
   , AnyVenue (Proxy :: Proxy "coinbase")
   ]

-- | Map of all supported venue names to its corresponding 'AnyVenue'
venueMap :: HM.HashMap Text AnyVenue
venueMap = HM.fromList $ map (\v@(AnyVenue p) -> (toS $ symbolVal p, v)) allVenues

-- | Lookup by 'Text' string in the map of all venues
venueLookup :: Text -> Maybe AnyVenue
venueLookup = (`HM.lookup` venueMap)



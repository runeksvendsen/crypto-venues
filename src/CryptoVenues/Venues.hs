{-# OPTIONS_GHC -fno-warn-orphans #-}
module CryptoVenues.Venues
( allVenues
, allVenuesText
, venueLookup
, AnyVenue(..)
)
where

import CryptoVenues.Internal.CPrelude
import CryptoVenues.Fetch.MarketBook

import CryptoVenues.Venues.Coinbase     as Coinbase       ()
import CryptoVenues.Venues.Bitstamp     as Bitstamp       ()
import qualified CryptoVenues.Venues.Bitfinex     as Bitfinex
import CryptoVenues.Venues.Bittrex      as Bittrex        ()
import CryptoVenues.Venues.Binance      as Binance        ()
import CryptoVenues.Venues.BinanceFut   as BinanceFut     ()

import qualified Data.HashMap.Strict   as HM


-- | Choose "aggregated mode". See: "CryptoVenues.Venues.Bitfinex".
instance MarketBook "bitfinex" where
   marketBook = Bitfinex.marketBook_Agg
   rateLimit = Bitfinex.marketBook_rateLimit

-- | Canonical list of all supported venues ('AnyVenue')
allVenues :: [AnyVenue]
allVenues =
   [ AnyVenue (Proxy :: Proxy "bitfinex")
   , AnyVenue (Proxy :: Proxy "bittrex")
   , AnyVenue (Proxy :: Proxy "binance")
   , AnyVenue (Proxy :: Proxy "binance-fut")
   , AnyVenue (Proxy :: Proxy "bitstamp")
   , AnyVenue (Proxy :: Proxy "coinbase")
   ]

-- | Canonical list of all supported venues ('Text')
allVenuesText :: [Text]
allVenuesText = map fst $ HM.toList venueMap

-- | Map of all supported venue names to its corresponding 'AnyVenue'
venueMap :: HM.HashMap Text AnyVenue
venueMap = HM.fromList $ map (\v@(AnyVenue p) -> (toS $ symbolVal p, v)) allVenues

-- | Lookup by 'Text' string in the map of all venues
venueLookup :: Text -> Maybe AnyVenue
venueLookup = (`HM.lookup` venueMap)

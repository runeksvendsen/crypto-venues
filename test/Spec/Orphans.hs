{-# OPTIONS_GHC -fno-warn-orphans #-}
module Spec.Orphans
(
)
where

import           CryptoVenues.Internal.CPrelude
import           CryptoVenues.Fetch.MarketBook
import           CryptoVenues.Fetch.EnumMarkets
import           CryptoVenues.Types.Market        (MarketList)
import qualified CryptoVenues.Venues.Bitfinex     as Bitfinex
import           Unsafe.Coerce                    (unsafeCoerce)


instance EnumMarkets "bitfinex-raw" where
   allMarkets = unsafeCoerce (allMarkets :: DataSrc (MarketList "bitfinex"))

instance MarketBook "bitfinex-raw" where
   marketBook = unsafeCoerce . Bitfinex.marketBook_Raw
   rateLimit = unsafeCoerce Bitfinex.marketBook_rateLimit

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CryptoVenues.Types.MarketSymbol
( MarketSymbol
, toMarketSymbol
)
where

import Prelude
import Protolude
import Text.Printf
import Servant.API  (ToHttpApiData)


-- | Venue-specific symbol for a particular market.
--
-- Used by 'CryptoVenues.Fetch.MarketBook.marketBook' to fetch
--  an order book for a particular market from a particular venue/exchange.
newtype MarketSymbol (venue :: Symbol) = MarketSymbol Text
    deriving (Eq, Show, Ord, PrintfArg, NFData, ToHttpApiData)


-- | Used to implement 'CryptoVenues.Fetch.EnumMarkets.allMarkets'.
--
-- A valid 'MarketSymbol' is one that can be used to fetch an order
--  book from an exchange using 'CryptoVenues.Fetch.MarketBook.marketBook'.
--
-- The 'Text' input is the symbol used by the exchange API to identify
--  a particular order book.
toMarketSymbol :: Text -> MarketSymbol venue
toMarketSymbol = MarketSymbol

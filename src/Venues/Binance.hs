module Venues.Binance where

import CPrelude
import Markets
import OrderBook
import Fetch

-- https://api.binance.com/api/v1/exchangeInfo

newtype ExchangeInfo = ExchangeInfo
   { symbols :: [BSymbol]
   }

data BSymbol = BSymbol
    { symbol               :: Text  -- e.g. "ETHBTC"
    , status               :: Text  -- =? "TRADING"
    , baseAsset            :: Text
    , baseAssetPrecision   :: Word
    , quoteAsset           :: Text
    , quotePrecision       :: Word
    }

fromSymbol :: BSymbol -> Market "binance"
fromSymbol BSymbol{..} = Market baseAsset quoteAsset symbol

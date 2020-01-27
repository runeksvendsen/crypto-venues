module CryptoVenues.Types.Market
( Market(..)
, MarketList(..)
, marketName
, MarketSymbol
)
where

import CryptoVenues.Types.MarketSymbol (MarketSymbol)
import Prelude
import Protolude
import Text.Printf


data Market (venue :: Symbol) = Market
   { miBase       :: Text
   , miQuote      :: Text
   , miApiSymbol  :: MarketSymbol venue
   } deriving (Eq, Generic)

instance NFData (Market venue)

-- | Only used in order to be able to create custom FromJSON instance
newtype MarketList (venue :: Symbol) = MarketList { getMarkets :: [Market venue] }
   deriving (Eq, Show)

instance KnownSymbol venue => Show (Market venue) where
   show mkt@Market{..} = printf template venueName (marketName mkt)
      where
         template = "<%s %s>"
         venueName = symbolVal (Proxy :: Proxy venue)

marketName :: Market venue -> String
marketName Market{..} = printf "%s/%s" miBase miQuote

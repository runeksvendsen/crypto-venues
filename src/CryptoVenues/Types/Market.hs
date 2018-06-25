--{-# LANGUAGE ExistentialQuantification #-}
module CryptoVenues.Types.Market where

import Prelude
import Protolude
--import CryptoVenues.Fetch.DataSrc
import OrderBook.Types
import Text.Printf
import qualified Servant.Client        as SC
import           Servant.API
import qualified Data.Aeson            as Json
import qualified Network.HTTP.Client   as HTTP


data Market (venue :: Symbol) = Market
   { miBase       :: Text
   , miQuote      :: Text
   , miApiSymbol  :: Text
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

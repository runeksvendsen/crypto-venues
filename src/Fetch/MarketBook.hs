{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Fetch.MarketBook
( MarketBook(..)
, AnyMarket(..)
, DataSrc(..)
, RateLimit
, fetchMarketBook
, AnyVenue(..)
, marketListAny
)
where

import CPrelude
import Fetch.DataSrc
import Fetch.EnumMarkets
import Types.RateLimit
import OrderBook.Types
import Types.Market
import qualified Servant.Client.Core.Reexport as S
import qualified Servant.Client        as SC
import           Servant.API
import qualified Data.Aeson            as Json
import qualified Network.HTTP.Client   as HTTP
import Data.Hashable


-- | Order book fetching
class EnumMarkets venue => MarketBook venue where
   marketBook :: Text -> DataSrc (SomeBook venue)  -- ^ Fetch a 'SomeBook' given a 'Market' 'miApiSymbol'
   rateLimit  :: DataSrc (RateLimit venue)         -- ^ How often can we fetch using 'marketBook'?

-- |
data AnyMarket =
   forall venue.
   MarketBook venue
      => AnyMarket { anyMarket :: Market venue }

instance NFData AnyMarket where
   rnf (AnyMarket mkt) = rnf mkt

instance Eq AnyMarket where
   (AnyMarket (m1 :: Market venue1)) == (AnyMarket (m2 :: Market venue2)) =
         miBase m1 == miBase m2
      && miQuote m1 == miQuote m2

instance Show AnyMarket where
   show (AnyMarket m) = show m

instance Hashable AnyMarket where
   hashWithSalt salt (AnyMarket mkt) =
      hashWithSalt salt [miBase mkt, miQuote mkt]

fetchMarketBook
   :: forall venue.
      MarketBook venue
   => Market venue
   -> AppM IO (AnyBook venue)
fetchMarketBook market@Market{..} = do
   man <- ask
   let handleErr = throwLeft . fmapL (Error (BookErr market))
   ob <- handleErr =<< liftIO (srcFetch man (marketBook miApiSymbol))
   return $ bookFromMarket market ob

-- | Convert a 'SomeBook' into an 'AnyBook' given a 'Market'
bookFromMarket
   :: forall venue.
      KnownSymbol venue
   => Market venue
   -> SomeBook venue
   -> AnyBook venue
bookFromMarket market sb =
   case someSymbolVal (toS $ miBase market) of
      SomeSymbol (Proxy :: Proxy base) ->
         case someSymbolVal (toS $ miQuote market) of
               SomeSymbol (Proxy :: Proxy quote) ->
                  AnyBook (fromSomeBook sb :: OrderBook venue base quote)

-- |
data AnyVenue
   = forall venue.
     MarketBook venue
   => AnyVenue (Proxy venue)

instance Show AnyVenue where
   show (AnyVenue p) = symbolVal p

instance Json.ToJSON AnyVenue where
   toJSON (AnyVenue p) = Json.toJSON (toS $ symbolVal p :: Text)

marketListAny
   :: MonadIO m
   => AnyVenue
   -> AppM m [AnyMarket]
marketListAny (AnyVenue p) =
   map AnyMarket <$> marketList p

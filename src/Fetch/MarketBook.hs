{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Fetch.MarketBook
( MarketBook(..)
, AnyMarket(..)
, fetchMarketBook
)
where

import CPrelude
import Fetch.DataSrc
import OrderBook.Types
import Markets.Types
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import           Servant.API
import qualified Data.Aeson            as Json
import qualified Network.HTTP.Client   as HTTP


-- | Use 'miApiSymbol' from 'Market' to fetch a 'SomeBook'
class KnownSymbol venue => MarketBook venue where
   marketBook :: Text -> DataSrc (SomeBook venue)

data AnyMarket = forall venue. MarketBook venue => AnyMarket (Market venue)

instance Eq AnyMarket where
   (AnyMarket (m1 :: Market venue1)) == (AnyMarket (m2 :: Market venue2)) =
      miBase m1 == miBase m2 &&
      miQuote m1 == miQuote m2 &&
      miApiSymbol m1 == miApiSymbol m2 &&
      isJust (sameSymbol (Proxy :: Proxy venue1) (Proxy :: Proxy venue2))

instance Show AnyMarket where
   show (AnyMarket m) = show m

fetchMarketBook
   :: forall venue.
      (KnownSymbol venue, MarketBook venue)
   => HTTP.Manager
   -> Market venue
   -> IO (Either SC.ServantError (AnyBook venue))
fetchMarketBook man market@Market{..} =
   fmap (bookFromMarket market) <$> srcFetch man (marketBook miApiSymbol)

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

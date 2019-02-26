{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
module CryptoVenues.Types.ABook where

import Prelude
import CryptoVenues.Internal.Prelude
import OrderBook.Types

import qualified CryptoVenues.Venues            ()
import           CryptoVenues.Fetch.MarketBook  (MarketBook)
import qualified OrderBook.Types                as OB


import qualified Data.Aeson                     as Json
import           Data.Aeson                     ((.=), (.:))
import           Data.Proxy                     (Proxy(..))
import           GHC.TypeLits                   (KnownSymbol, SomeSymbol(..)
                                                , symbolVal, someSymbolVal
                                                )
import           Data.Maybe                     (fromMaybe)

-- | Just some order book
data ABook =
    forall venue base quote.
    ( KnownSymbol venue, KnownSymbol base, KnownSymbol quote
    , MarketBook venue)
       => ABook (OrderBook venue base quote)

instance Show ABook where
    show (ABook ob) =
        toS $ abBase ob <> "/" <> abQuote ob <> " (" <> abVenue ob <> ")"

instance Eq ABook where
    (ABook ob1) == (ABook ob2) =
        case ob1 of
            (_ :: OrderBook venue1 base1 quote1) ->
                case ob2 of
                    (_ :: OrderBook venue2 base2 quote2) ->
                        case sameSymbol (Proxy :: Proxy venue1) (Proxy :: Proxy venue2) of
                            Nothing -> False
                            Just _  ->
                                case sameSymbol (Proxy :: Proxy base1) (Proxy :: Proxy base2) of
                                    Nothing -> False
                                    Just _  ->
                                        case sameSymbol (Proxy :: Proxy quote1) (Proxy :: Proxy quote2) of
                                            Nothing -> False
                                            Just _  -> True

abBase
    :: forall base a b c. KnownSymbol base
    => a (b :: Symbol) (base :: Symbol) (c :: Symbol)
    -> Text
abBase _ = toS $ symbolVal (Proxy :: Proxy base) :: Text

abQuote
    :: forall quote a b c. KnownSymbol quote
    => a (b :: Symbol) (c :: Symbol) (quote :: Symbol)
    -> Text
abQuote _ = toS $ symbolVal (Proxy :: Proxy quote) :: Text


abVenue
    :: forall venue a b c. KnownSymbol venue
    => a (venue :: Symbol) (b :: Symbol) (c :: Symbol)
    -> Text
abVenue _ =
    toS $ symbolVal (Proxy :: Proxy venue) :: Text

instance Json.ToJSON ABook where
    toJSON (ABook ob) = case ob of
        (_ :: OB.OrderBook venue base quote) -> Json.object
            [ "bids"  .= Json.toJSON (OB.obBids ob)
            , "asks"  .= Json.toJSON (OB.obAsks ob)
            , "venue" .= Json.toJSON (symbolVal (Proxy :: Proxy venue))
            , "base"  .= Json.toJSON (symbolVal (Proxy :: Proxy base))
            , "quote" .= Json.toJSON (symbolVal (Proxy :: Proxy quote))
            ]

instance Json.FromJSON ABook where
    parseJSON = Json.withObject "ABook" $ \obj -> do
        venue <- obj .: "venue"
        base  <- obj .: "base"
        quote <- obj .: "quote"
        case someSymbolVal base of
            SomeSymbol (Proxy :: Proxy base) -> case someSymbolVal quote of
                SomeSymbol (Proxy :: Proxy quote) -> do
                    case marketBookVenue venue of
                        Nothing -> Prelude.fail $ "Unknown venue: " ++ venue
                        Just (MarketBookVenue (Proxy :: Proxy venue)) -> do
                            buySide  <- obj .: "bids"
                            sellSide <- obj .: "asks"
                            let ob = OB.OrderBook buySide sellSide
                            return $ ABook (ob :: OB.OrderBook venue base quote)

data MarketBookVenue =
    forall venue. MarketBook venue
    => MarketBookVenue (Proxy venue)

marketBookVenue :: String -> Maybe MarketBookVenue
marketBookVenue "bitfinex" = Just . MarketBookVenue $ Proxy @"bitfinex"
marketBookVenue "bittrex"  = Just . MarketBookVenue $ Proxy @"bittrex"
marketBookVenue "binance"  = Just . MarketBookVenue $ Proxy @"binance"
marketBookVenue "bitstamp" = Just . MarketBookVenue $ Proxy @"bitstamp"
marketBookVenue "coinbase" = Just . MarketBookVenue $ Proxy @"coinbase"
marketBookVenue _          = Nothing

toABook :: MarketBook venue => AnyBook venue -> ABook
toABook (AnyBook ob) = ABook ob

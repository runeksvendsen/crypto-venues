{-# LANGUAGE ExistentialQuantification #-}
module CryptoVenues.Types.ABook where

import Prelude
import CryptoVenues.Internal.Prelude
import CryptoVenues.Fetch.MarketBook
import OrderBook.Types


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

{-|
Module      : CryptoVenues.Venues.Bitfinex.ResponseMapping
Description : Mappings from Bitfinex internal symbols to official symbols

Bitfinex uses its own three-letter symbols for cryptocurrencies. This module
uses Bitfinex' API to return a mapping of Bitfinex' internal symbols to the
symbols used by everyone else.

-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CryptoVenues.Venues.Bitfinex.Mapping
( ApiMapping
, Response
, Mapping
, responseMapping
, normalize
, BitfinexOverlappingMarkets
)
where

import Prelude
import CryptoVenues.Internal.CPrelude (Generic, fromMaybe, Exception, groupOnOrd, NFData)
import CryptoVenues.Types.Market (Market(..), MarketList(MarketList), getMarkets)

import           Servant.API (JSON, type (:>), Get)
import           Data.Vector (Vector)
import qualified Data.Aeson as Json
import qualified Data.Vector as Vec
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Unsafe.Coerce (unsafeCoerce)
import qualified Control.Monad.Catch as Catch
import Data.List (unlines)


-- | Docs: https://docs.bitfinex.com/reference#rest-public-conf
--   URL: https://api-pub.bitfinex.com/v2/conf/pub:map:currency:sym
type ApiMapping
   = "v2"
   :> "conf"
   :> "pub:map:currency:sym"
   :> Get '[JSON] Response

-- An outer array of length one which contains an array of tuples (two-element arrays in JSON)
newtype Response = Response (Vector (Vector ResponseMapping))
    deriving (Generic)

-- | A map from Bitfinex' internal symbol to the symbol used by everyone else
responseMapping :: Response -> Mapping
responseMapping =
    Mapping . toMap
            . ignoreTestSymbols
            . ignorePerpetualFutures
            . map getResponseMapping
            . Vec.toList
            . Vec.head
            . getResponse
  where
    toMap = Map.fromList
    getResponse (Response vec) = vec
    -- convert the "to" component (second element in the tuple) to upper case
    getResponseMapping (ResponseMapping mapping) = fmap T.toUpper mapping
    -- Symbols ending in "F0" are perpetual futures contracts, and the API says that
    --  e.g. "BTCF0" is equivalent to "BTC", but we do not consider a perpetual
    --  futures contract to be equal to its underlying asset.
    ignorePerpetualFutures = filter (not . isPerpetualFuture . fst)
    isPerpetualFuture symbol = "F0" `T.isSuffixOf` symbol
    ignoreTestSymbols = filter (not . isTestSymbol . fst)
    isTestSymbol = ("TEST" `T.isPrefixOf`) -- Bitfinex claims e.g. TESTBTC is the same as BTC, but we disagree

instance Json.FromJSON ResponseMapping
instance Json.FromJSON Response

-- | The API returns a mapping as a tuple (two-element JSON array)
--    where the first element is "from" and the second is "to".
newtype ResponseMapping = ResponseMapping (T.Text, T.Text) -- ^ (from, to)
    deriving (Generic)

newtype Mapping = Mapping (Map.Map T.Text T.Text)
    deriving (Eq, Show, Ord, NFData)

-- | Convert an internal Bitfinex symbol into an official symbol
normalizeMarket
    :: Mapping
    -> Market "bitfinex_internal" -- ^ Contains a Bitfinex internal symbol (e.g. "UST")
    -> Market "bitfinex" -- ^ Contains an official symbol (e.g. "USDT")
normalizeMarket (Mapping mapping) market = unsafeCoerce $
    market
        { miBase = normalizeSymbol (miBase market)
        , miQuote = normalizeSymbol (miQuote market)
        }
  where
    normalizeSymbol symbol =
        fromMaybe symbol (Map.lookup symbol mapping)

-- | Convert internal Bitfinex symbols into official symbols
normalize
    :: Catch.MonadThrow m
    => Mapping
    -> MarketList "bitfinex_internal"  -- ^ Contains Bitfinex internal symbols (e.g. "UST")
    -> m (MarketList "bitfinex")  -- ^ Contains official symbols (e.g. "USDT")
normalize mapping markets =
    if not (null overlappingMarkets)
        then Catch.throwM $ BitfinexOverlappingMarkets relevantInternalMarkets overlappingMarkets (Mapping relevantMapping')
        else pure $ MarketList newMarketList
  where
    newMarketList = map (normalizeMarket mapping) (getMarkets markets)

    overlappingMarkets = getOverlappingMarkets newMarketList

    getOverlappingMarkets =
        filter ((> 1) . length) . groupOnOrd (\mkt -> (miBase mkt, miQuote mkt))

    relevantMapping' =
        let relevantNormalizedSymbols = concatMap (\m -> [miBase m, miQuote m]) $ concat overlappingMarkets
            (Mapping mapping') = mapping
        in Map.filter (`elem` relevantNormalizedSymbols) mapping'

    relevantInternalMarkets =
        filter
            (\m -> not $ Set.fromList [miBase m, miQuote m] `Set.disjoint` Map.keysSet relevantMapping')
            (getMarkets markets)

-- | Exception thrown in case the process of renaming internal Bitfinex
--    symbols into official symbols results in two different markets
--    with the same "base" and "quote" symbol.
--   This is considered a bug in the Bitfinex API and is thefore fatal.
data BitfinexOverlappingMarkets = BitfinexOverlappingMarkets
    [Market "bitfinex_internal"] -- ^ Relevant subset of input markets
    [[Market "bitfinex"]] -- ^ Overlapping markets using official symbols
    Mapping -- ^ Part of mapping that produced overlapping markets

instance Show BitfinexOverlappingMarkets where
   show (BitfinexOverlappingMarkets markets overlappingMarkets mapping) =
      let githubUrl =
              "https://github.com/runeksvendsen/crypto-venues/issues/new?title=Bug:+overlapping+Bitfinex+markets"
          details = unlines
              [ "Overlapping markets: " <> show overlappingMarkets
              , "Input markets: " <> show markets
              , "Mapping: " <> show mapping
              ]
      in unlines
      [ "BUG: Bitfinex market overlap"
      , "Please report this issue at the following GitHub URL, and add the below info to the issue."
      , "GitHub issue URL: " <> githubUrl
      , "DETAILS:"
      , details
      ]

instance Exception BitfinexOverlappingMarkets

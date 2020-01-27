{-|
Module      : CryptoVenues.Venues.Bitfinex
Description : Bitfinex implementation for 'EnumMarkets' and functions to implement 'MarketBook'

The Bitfinex order book API has two modes, and it's necessary to choose one.

/Raw/ mode returns raw orders from the order book, so that it's possible to distinguish between
multiple orders of the same price and quantity. This mode has 100% price precision,
but only returns a small part of the order book (since Bitfinex doesn't
allow fetching more than 100 orders from an order book).

/Aggregation/ mode aggregates multiple almost-same-priced orders into a single order.
This mode thus loses price precision but enables fetching a larger part of the order book.

__Raw mode:__

* Advantage: 100% price precision.
* Disadvantage: only tiny part of order book for large order books (e.g. ~0.01% price range for ETH/BTC)

__Aggregation mode:__

* Advantage: larger part of order book, even for large order books (e.g. ~5% price range for ETH/BTC)
* Disadvantage: less than 100% precision (seemingly 99.9% precision)

Since there are multiple useful implementations for 'MarketBook' you'll have to implement it
yourself.

To choose "raw mode":

> instance MarketBook "bitfinex" where
>    marketBook = marketBook_Raw
>    rateLimit = marketBook_rateLimit

To choose "aggregation mode":

> instance MarketBook "bitfinex" where
>    marketBook = marketBook_Agg
>    rateLimit = marketBook_rateLimit

-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module CryptoVenues.Venues.Bitfinex
( marketBook_rateLimit
, marketBook_Raw
, marketBook_Agg
, OB.SomeBook
)
where

import CryptoVenues.Internal.CPrelude     hiding (asks)
import qualified OrderBook.Types          as OB
import CryptoVenues.Fetch
import CryptoVenues.Types.Market
import CryptoVenues.Types.MarketSymbol
import CryptoVenues.Venues.Common.ScientificOrder

import qualified Servant.Client        as SC
import Servant.API
import qualified Data.Aeson   as Json
import qualified Data.Aeson.Types   as Json
import           Data.Vector  (Vector)
import qualified Data.Vector  as Vec
import qualified Data.Text as T


-- NB: Two things you should know about Bitfinex' v2 API before proceeding:
--
--  1) Available markets are enumerated using the "tickers" API. This API also
--      includes so-called "funding currencies" (which are not markets).
--     "Funding currencies" are encoded in an array with a different length than that
--      of the market tickers ("trading pair codes"), which means we have to manually
--      grab the first element of this array (which always contains the symbol,
--      regardless of whether it's a "trading pair code" (market) or "funding currency").
--
--  2) The sell and buy orders in an order book are encoded as a
--      single list of orders, in which the buy orders are those with a positive
--      quantity and the sell orders those with a negative quantity.

apiUrl :: BaseUrl
apiUrl = BaseUrl Https "api-pub.bitfinex.com" 443 ""

instance EnumMarkets "bitfinex" where
   allMarkets = DataSrc apiUrl (clientM $ Just "ALL")
      where
         clientM = SC.client (Proxy :: Proxy ApiMarkets)

-- | Used to implemented 'rateLimit' for @"bitfinex"@
marketBook_rateLimit :: DataSrc (RateLimit "bitfinex")
marketBook_rateLimit = DataSrc apiUrl (return 0.5)
-- Orderbook rate limit: 30 req/min (https://docs.bitfinex.com/v2/reference#rest-public-books)

-- | Used to implemented 'marketBook' for @"bitfinex"@.
--   Other option: 'marketBook_Agg'
marketBook_Raw :: MarketSymbol "bitfinex" -> DataSrc (OB.SomeBook "bitfinex")
marketBook_Raw = fmap fromOrderBookRaw . marketBook_Raw'

marketBook_Raw' :: MarketSymbol "bitfinex" -> DataSrc (OrderBookRaw "bitfinex")
marketBook_Raw' apiSymbol =
   DataSrc apiUrl cm
    where
   cm = clientM apiSymbol (Just 100)
   clientM = SC.client (Proxy :: Proxy ApiBooksRaw)

-- | Used to implemented 'marketBook' for @"bitfinex"@.
--   Other option: 'marketBook_Raw'.
marketBook_Agg :: MarketSymbol "bitfinex" -> DataSrc (OB.SomeBook "bitfinex")
marketBook_Agg = fmap fromOrderBookAgg . marketBook_Agg'

marketBook_Agg' :: MarketSymbol "bitfinex" -> DataSrc (OrderBookAgg "bitfinex")
marketBook_Agg' apiSymbol =
   DataSrc apiUrl cm
    where
   cm = clientM apiSymbol (Just 100)
   clientM = SC.client (Proxy :: Proxy ApiBooksAgg)

instance Json.FromJSON (OrderBookRaw "bitfinex") where
   parseJSON val =
      Json.parseJSON val >>= (either fail return . fmap OrderBookRaw . toSomeBook)

instance Json.FromJSON (OrderBookAgg "bitfinex") where
   parseJSON val =
      Json.parseJSON val >>= (either fail return . fmap OrderBookAgg . toSomeBook . Vec.map toOrderRaw)

-- Take a Vector of orders -- as returned by the "books" API -- and
--  turn them into a 'OB.SomeBook'.
toSomeBook :: Vector BitfinexOrder -> Either String (OB.SomeBook "bitfinex")
toSomeBook bookOrders = do
    bids <- maybe (Left $ "Invalid buy order(s)" ++ show bookOrders) Right bidsM
    asks <- maybe (Left $ "Invalid sell order(s)" ++ show bookOrders) Right asksM
    OB.mkSomeBook bids asks
  where
    asksM = traverse toSellOrder $ Vec.filter (not . isBuyOrder) bookOrders
    -- HACK: ignore zero-price buy orders from Bitfinex
    bidsM = traverse toBuyOrder . Vec.filter nonZeroPrice . Vec.filter isBuyOrder $ bookOrders
    -- NB: a sell order is encoded as an order with negative quantity
    toSellOrder = toSomeOrder (* (-1))
    toBuyOrder = toSomeOrder id

-- | (order_id, price, quantity)
type BitfinexOrder = (Word64, Scientific, Scientific)

isBuyOrder :: BitfinexOrder -> Bool
isBuyOrder (_, _, qty) = qty > 0

nonZeroPrice :: BitfinexOrder -> Bool
nonZeroPrice (_, price, _) = price /= 0

-- | (price, count, quantity)
type BitfinexOrderSet = (Scientific, Int, Scientific)

-- | Compatibility function for re-using logic for
--    non-aggregated orders with aggregated orders
toOrderRaw :: BitfinexOrderSet -> BitfinexOrder
toOrderRaw (price, count, quantity) =
   (0, price, quantity * fromIntegral count)

-- |
toSomeOrder
   :: (Rational -> Rational)  -- ^ Modify quantity
   -> BitfinexOrder
   -> Maybe OB.SomeOrder
toSomeOrder f (_, price, quantity) =
   OB.mkSomeOrder (f $ toRational quantity) (toRational price)

newtype OrderBookRaw (venue :: Symbol) =
   OrderBookRaw (OB.SomeBook venue)

newtype OrderBookAgg (venue :: Symbol) =
   OrderBookAgg (OB.SomeBook venue)

fromOrderBookRaw :: OrderBookRaw venue -> OB.SomeBook venue
fromOrderBookRaw (OrderBookRaw someBook) = someBook

fromOrderBookAgg :: OrderBookAgg venue -> OB.SomeBook venue
fromOrderBookAgg (OrderBookAgg someBook) = someBook

-- | don't aggregate orders.
--   * advantage: 100% price precision.
--   * disadvantage: only tiny part of order book for large order books
--                   (e.g. ~0.01% price range for ETH/BTC)
type NoAggregate = "R0"

-- | aggregate orders.
--   * advantage: larger part of order book, even for large order books
--                (e.g. ~5% price range for ETH/BTC)
--   * disadvantage: less than 100% precision (seemingly 99.9% precision)
type AggregateLevel1 = "P1"

-- | Docs: https://docs.bitfinex.com/v2/reference#rest-public-books
--   Example: https://api-pub.bitfinex.com/v2/book/tBTCUSD/R0?len=100
type ApiBooksRaw
   = "v2"
   :> "book"
   :> Capture "symbol" (MarketSymbol "bitfinex")
   :> NoAggregate
   :> QueryParam "len" Word
   :> Get '[JSON] (OrderBookRaw "bitfinex")

-- | Docs: https://docs.bitfinex.com/v2/reference#rest-public-books
--   Example: https://api-pub.bitfinex.com/v2/book/tBTCUSD/P1?len=100
type ApiBooksAgg
   = "v2"
   :> "book"
   :> Capture "symbol" (MarketSymbol "bitfinex")
   :> AggregateLevel1
   :> QueryParam "len" Word
   :> Get '[JSON] (OrderBookAgg "bitfinex")

-- | Docs: https://docs.bitfinex.com/v2/reference#rest-public-tickers
--   Example: https://api-pub.bitfinex.com/v2/tickers?symbols=ALL
type ApiMarkets
   = "v2"
   :> "tickers"
   :> QueryParam "symbols" Text
   :> Get '[JSON] (MarketList "bitfinex")

instance Json.FromJSON (MarketList "bitfinex") where
   parseJSON = Json.withArray "bitfinex MarketList" $ \vector ->
      -- NB: We ignore symbols not starting with "t".
      --     E.g.: "funding symbols", which start with "f".
      let isTradingPair symbol = T.take 1 symbol == "t"
          mkPairCodeVector =
              traverse (Json.withArray "bitfinex ticker list" getPairCode)
      in do
         pairCodeVector <- Vec.filter isTradingPair <$> mkPairCodeVector vector
         marketVector <- traverse (either fail return . parsePairCode) pairCodeVector
         return $ MarketList (Vec.toList marketVector)

parsePairCode :: Text -> Either String (Market "bitfinex")
parsePairCode currPair =
   let errorStr = "Invalid trading pair code: " ++ toS currPair
   in maybe (Left errorStr) Right (fromPairCode currPair)

-- | Get the trading pair code from a single item
--    in the array returned by the "tickers" API
getPairCode
   :: Vec.Vector Json.Value
   -> Json.Parser T.Text
getPairCode vec =
   -- symbol is always first item in the array
   let getSymbol = (Vec.! 0)
   in Json.withText "bitfinex trading pair code" return (getSymbol vec)

-- | Bitfinex "(trading) pair code" string format (deduced through trial and error):
-- Either (in descending priority):
--  a) exactly six charaters in length:
--       * first three characters comprise base currency
--       * following three characters comprise quote currency
--  b) containing exactly one colon (':') character:
--       * base/quote currency separated by colon (':') character
fromPairCode
   :: T.Text
   -> Maybe (Market "bitfinex")
fromPairCode pairCode
   | T.length cutPairCode == 6 =
      mkMarket (T.toUpper $ T.take 3 cutPairCode) (T.toUpper $ T.takeEnd 3 cutPairCode)
   | [baseSymbol, quoteSymbol] <- T.split (== ':') cutPairCode =
      mkMarket baseSymbol quoteSymbol
   | otherwise = Nothing
  where
   -- The first character is 't' (for "Trading pair")
   cutPairCode = discardFirstChar pairCode
   discardFirstChar symbol = T.takeEnd (T.length symbol - 1) symbol
   mkMarket base quote =
      Just $ Market
            { miBase       = base
            , miQuote      = quote
            , miApiSymbol  = toMarketSymbol pairCode
            }

{-# OPTIONS_GHC -fno-warn-orphans #-}
module CryptoVenues.Venues.Bitfinex
()
where

import CryptoVenues.Internal.CPrelude     hiding (asks)
import qualified OrderBook.Types          as OB
import CryptoVenues.Fetch
import CryptoVenues.Types.Market
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

instance MarketBook "bitfinex" where
   marketBook apiSymbol = DataSrc apiUrl cm
      where cm = clientM apiSymbol "R0" (Just 100)
            clientM = SC.client (Proxy :: Proxy (ApiBooks base quote))
   rateLimit = DataSrc apiUrl (return 0.5)
-- Orderbook rate limit: Ratelimit: 30 req/min (https://docs.bitfinex.com/v2/reference#rest-public-books)

instance Json.FromJSON (OB.SomeBook "bitfinex") where
   parseJSON val =
      Json.parseJSON val >>= (either fail return . toSomeBook)

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

-- |
toSomeOrder
   :: (Rational -> Rational)  -- ^ Modify quantity
   -> BitfinexOrder
   -> Maybe OB.SomeOrder
toSomeOrder f (_, price, quantity) =
   OB.mkSomeOrder (f $ toRational quantity) (toRational price)

-- | Docs: https://docs.bitfinex.com/v2/reference#rest-public-books
--   Example: https://api-pub.bitfinex.com/v2/book/tBTCUSD/R0?len=100
type ApiBooks base quote
   = "v2"
   :> "book"
   :> Capture "symbol" Text
   :> Capture "precision" Text
   :> QueryParam "len" Word
   :> Get '[JSON] (OB.SomeBook "bitfinex")

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
            , miApiSymbol  = pairCode
            }

module Venues.Common.StringArrayOrder where

import CPrelude
import OrderBook.Types
import qualified Money
import qualified Data.Scientific as Sci
import Control.Monad.Fail
import qualified Data.Aeson.Types   as Json

type PriceStr = String
type QtyStr = String

parseOrderStr :: -- forall base quote. (KnownSymbol base, KnownSymbol quote)
              PriceStr -> QtyStr -> Json.Parser (Order base quote)
parseOrderStr price qty =
   Order <$> parseStr "quantity" qty Money.dense
         <*> parseStr "price" price Money.exchangeRate
   where
   -- market = symbolVal (Proxy :: Proxy base) <> "/" <> symbolVal (Proxy :: Proxy quote)
   parseStr :: String -> String -> (Rational -> Maybe a) -> Json.Parser a
   parseStr name val conv =
      maybe (fail $ "Bad order " <> name <> ": " <> val) return $ do
          sci <- readMaybe val
          conv (toRational sci)

parseSomeOrderStr :: PriceStr -> QtyStr -> Json.Parser SomeOrder
parseSomeOrderStr price qty = fromOrder <$> parseOrderStr price qty

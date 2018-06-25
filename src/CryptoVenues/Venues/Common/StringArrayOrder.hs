module CryptoVenues.Venues.Common.StringArrayOrder where

import CryptoVenues.Internal.CPrelude
import OrderBook.Types
import qualified Money
import qualified Data.Scientific as Sci
import Control.Monad.Fail
import qualified Data.Aeson.Types   as Json

type PriceStr = String
type QtyStr = String

parseOrderStr :: PriceStr -> QtyStr -> Either String (Order base quote)
parseOrderStr price qty =
   Order <$> parseStr "quantity" qty Money.dense
         <*> parseStr "price" price Money.exchangeRate

parseStr :: String -> String -> (Rational -> Maybe a) -> Either String a
parseStr name val conv =
   maybe (Left $ "Bad order " <> name <> ": " <> val) Right $ do
       sci :: Sci.Scientific <- readMaybe val
       conv (toRational sci)

parseSomeOrderStr :: PriceStr -> QtyStr -> Either String SomeOrder
parseSomeOrderStr price qty = fromOrder <$> parseOrderStr price qty

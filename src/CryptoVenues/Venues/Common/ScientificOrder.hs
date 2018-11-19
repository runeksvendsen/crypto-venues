{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CryptoVenues.Venues.Common.ScientificOrder
( parseSomeOrderSci
, Sci.Scientific
, QuotedScientific
)
where

import CryptoVenues.Internal.CPrelude
import OrderBook.Types
import qualified Money
import qualified Data.Scientific as Sci
import qualified Data.Aeson      as Json

-- | A 'Sci.Scientific' that can be parsed from a quoted string,
--  e.g. "\"1.27\""
newtype QuotedScientific
   = QuotedScientific Sci.Scientific
      deriving (Eq, Fractional, Num, Ord, Real, RealFrac)

instance Show QuotedScientific where
   show (QuotedScientific sci) = show sci

instance Json.FromJSON QuotedScientific where
   parseJSON val = do
      str :: String <- Json.parseJSON val
      QuotedScientific <$>
         maybe (fail $ "Invalid Scientific: " ++ show str) return
            (readMaybe str :: Maybe Sci.Scientific)

parseOrderSci :: QuotedScientific -> QuotedScientific -> Either String (Order base quote)
parseOrderSci price qty =
   Order <$> parseSci "quantity" qty Money.dense
         <*> parseSci "price" price Money.exchangeRate

parseSci :: String -> QuotedScientific -> (Rational -> Maybe a) -> Either String a
parseSci name sci conv =
   maybe (Left $ "Bad order " <> name <> ": " <> show sci) Right $ do
       conv (toRational sci)

parseSomeOrderSci :: QuotedScientific -> QuotedScientific -> Either String SomeOrder
parseSomeOrderSci price qty = fromOrder <$> parseOrderSci price qty

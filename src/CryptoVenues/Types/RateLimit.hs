{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CryptoVenues.Types.RateLimit
( RateLimit )
where

import Prelude
import GHC.TypeLits                       (Symbol)
import Text.Printf                        (printf)
import Data.Ratio                         (numerator, denominator)
import qualified Data.Time.Units          as Time


instance Time.TimeUnit (RateLimit venue) where
   toMicroseconds   = round . (* 1e6) . (1 /) . rateLimit
   fromMicroseconds = RateLimit . (1 /) . (/ 1e6) . toRational

-- | Per-second request rate
newtype RateLimit (venue :: Symbol) = RateLimit { rateLimit :: Rational }
   deriving (Eq, Enum, Fractional, Num, Ord, Real, RealFrac)

instance Show (RateLimit venue) where
   show RateLimit{..} = printf "%d/%ds (%.3g/s)"
      (numerator rateLimit)
      (denominator rateLimit)
      (realToFrac rateLimit :: Double)

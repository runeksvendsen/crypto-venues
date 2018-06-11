{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types.RateLimit
( RateLimit )
where

import CPrelude
import qualified Control.Concurrent    as C
import qualified Data.Time.Units    as Time


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

delay :: RateLimit venue -> IO ()
delay (RateLimit rat) = C.threadDelay
   (round $ rat * 1e6) -- Delay for a number of microseconds

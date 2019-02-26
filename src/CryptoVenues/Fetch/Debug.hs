module CryptoVenues.Fetch.Debug
( allBooks
, allBooksSimple
, ABook(..)
)
where

import           Prelude
import           CryptoVenues.Types.ABook
import           CryptoVenues.Types.Market
import qualified CryptoVenues.Fetch.EnumMarkets    as EnumMarkets
import           CryptoVenues.Fetch.MarketBook     (fetchMarketBook)
import qualified CryptoVenues.Venues               as Venues
import qualified CryptoVenues.Types.AppM           as AppM

import           Data.Proxy                        (Proxy(..))
import           GHC.TypeLits                      (KnownSymbol, symbolVal)
import           Data.List                         ((\\))
import qualified Data.Text                         as T
import qualified Control.Monad.Parallel            as Par


allBooksSimple :: AppM.AppM IO [Either AppM.Error [ABook]]
allBooksSimple = allBooks (Proxy :: Proxy "USD") maxBound

-- | Fetch books, in parallel, from all venues
allBooks
    :: KnownSymbol numeraire
    => Proxy numeraire  -- ^ Only used for debugging (must exist though)
    -> Word
    -> AppM.AppM IO [Either AppM.Error [ABook]]
allBooks p numObLimit =
    Par.forM Venues.allVenues (AppM.evalAppM . fetchVenueBooks p numObLimit)

-- | Fetch books from a single venue
--  DEBUG: limit number of fetched books to 'numObLimit'
fetchVenueBooks
   :: forall numeraire.
      (KnownSymbol numeraire)
   => Proxy numeraire
   -> Word
   -> Venues.AnyVenue
   -> AppM.AppM IO [ABook]
fetchVenueBooks _ numObLimit (Venues.AnyVenue (_ :: Proxy venue)) = do
    allMarkets :: [Market venue] <- EnumMarkets.marketList
    -- Begin DEBUG stuff
    let btcEth = ["BTC", "ETH"]
        numeraire = T.pack $ symbolVal (Proxy :: Proxy numeraire)
        numeraireLst = filter (\mkt -> miBase mkt `elem` btcEth && miQuote mkt == numeraire) allMarkets
        markets = take (fromIntegral numObLimit - length numeraireLst) (allMarkets \\ numeraireLst)
        marketList = numeraireLst ++ markets
    -- End DEBUG stuff
    map toABook <$> mapM fetchMarketBook marketList

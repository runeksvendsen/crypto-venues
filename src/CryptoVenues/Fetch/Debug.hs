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
    -> Int
    -> AppM.AppM IO [Either AppM.Error [ABook]]
allBooks p numObLimit =
    Par.forM Venues.allVenues (AppM.evalAppM . fetchVenueBooks p numObLimit)

-- | Fetch books from a single venue
--  DEBUG: limit number of fetched books to 'numObLimit'
fetchVenueBooks
   :: forall numeraire.
      (KnownSymbol numeraire)
   => Proxy numeraire
   -> Int
   -> Venues.AnyVenue
   -> AppM.AppM IO [ABook]
fetchVenueBooks numeraire numObLimit (Venues.AnyVenue (_ :: Proxy venue)) = do
    allMarkets :: [Market venue] <- EnumMarkets.marketList
    let marketList = debugFilterMarkets numeraire numObLimit allMarkets
    map toABook <$> mapM fetchMarketBook marketList


-- | Reduce the maximum number of fetched order books. Used for testing.
debugFilterMarkets
    :: forall numeraire venue.
       KnownSymbol numeraire
    => Proxy numeraire
    -> Int
    -> [Market venue]
    -> [Market venue]
debugFilterMarkets _ numObLimit allMarkets =
    numeraireLst ++ markets
  where
    btcEth = ["BTC", "ETH"]
    numeraire = T.pack $ symbolVal (Proxy :: Proxy numeraire)
    numeraireLst = filter (\mkt -> miBase mkt `elem` btcEth && miQuote mkt == numeraire) allMarkets
    numElem = fromIntegral numObLimit - length numeraireLst
    markets = take numElem (allMarkets \\ numeraireLst)

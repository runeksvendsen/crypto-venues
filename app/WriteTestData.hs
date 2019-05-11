module Main where

import           Prelude
import qualified CryptoVenues
import qualified CryptoVenues.Fetch.Debug       as Fetch
import qualified CryptoVenues.Types.AppM        as AppM

import           Data.Proxy                     (Proxy(..))
import qualified Network.HTTP.Client.TLS        as HTTPS
import qualified Network.HTTP.Client            as HTTP
import qualified Control.Logging                as Log
import           Options.Applicative
import qualified Data.Aeson                     as Json
import qualified Data.Text                      as T
import           Control.Error                  (lefts, rights)
import           Control.Monad                  (forM_)


-- | In which currency do we want to measure liquidity?
type Numeraire = "USD"

logLevel = Log.LevelInfo
maxRetries = 15

main :: IO ()
main = do
    args <- execParser opts
    case progCommand args of
        ListVenues    -> forM_ CryptoVenues.allVenuesText (putStrLn . T.unpack)
        Write outFile -> fetchWriteBooks outFile (obCount args)
  where
    throwErrM ioA = ioA >>= either (error . show) return
    fetchWriteBooks outputFile obCount = do
        man <- HTTP.newManager HTTPS.tlsManagerSettings
        booksE <- throwErrM $ withLogging $ AppM.runAppM man maxRetries $
            Fetch.allBooks (Proxy :: Proxy Numeraire) obCount
        -- Log errors
        forM_ (lefts booksE) (\err -> putStrLn $ "Errors: " ++ show err)
        -- Write JSON books
        let books = concat $ rights booksE
        if books /= []
            then do
                Json.encodeFile outputFile books
                putStrLn $ "Wrote " ++ show outputFile
            else
                putStrLn $ "Fatal error: no order books fetched (all errored)"

withLogging :: IO a -> IO a
withLogging ioa = Log.withStderrLogging $ do
    Log.setLogLevel logLevel
    Log.setLogTimeFormat "%T.%3q"
    ioa

opts :: ParserInfo Options
opts = info options $
     fullDesc
  <> progDesc "Write test data to specified file"
  <> header "Fetch & write test data"

data Options = Options
  { progCommand :: Command
  , obCount     :: Word
  }

data Command
  = Write FilePath
  | ListVenues

commandOpt :: Parser Command
commandOpt = commandWriteOpt <|> commandListOpt

commandWriteOpt :: Parser Command
commandWriteOpt = Write <$> strOption
  (  long "write"
  <> metavar "FILEPATH"
  <> help "Write test data order books to this file" )

commandListOpt :: Parser Command
commandListOpt = flag' ListVenues
  (  long "list-venues"
  <> help "Print supported venues" )

options :: Parser Options
options = Options
      <$> commandOpt
      <*> obCount'

targetFile' :: Parser String
targetFile' = strOption
  (  long "file"
  <> metavar "FILEPATH"
  <> help "File path to write test data to" )

obCount' :: Parser Word
obCount' = option auto
  ( long "ob-count"
  <> short 'c'
  <> value maxBound
  <> metavar "ORDERBOOK_COUNT"
  <> help "Limit the number of fetched order books" )

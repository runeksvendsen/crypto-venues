module Spec.RetryAfter
( spec
)
where

import           CryptoVenues.Internal.CPrelude
import qualified Spec.RetryAfter.Server         as Server
import qualified CryptoVenues.Types.Error       as Error

import           Test.Hspec
import qualified Network.HTTP.Client            as HTTP
import qualified Servant.Client                 as SC
import qualified Control.Concurrent             as Conc
import qualified Test.QuickCheck                as QC


spec :: HTTP.Manager -> Spec
spec man = beforeAll startServer $
    describe "uses Retry-After header delay for" $ do
        testStatusCode man 429
        testStatusCode man 418

testStatusCode
    :: HTTP.Manager
    -> Word
    -> SpecWith (Word -> Word -> SC.ClientM b, BaseUrl)
testStatusCode man statusCode =
    it ("HTTP status " ++ show statusCode) $ \setupInfo ->
        QC.property (testRequest man statusCode setupInfo)

testRequest
    :: HTTP.Manager
    -> Word
    -> (Word -> Word -> SC.ClientM b, BaseUrl)
    -> Word
    -> IO ()
testRequest man statusCode (client, baseUrl) secs = do
    resE <- SC.runClientM (client statusCode secs) (SC.ClientEnv man baseUrl Nothing)
    let fetchErr (Left err) = Error.fromServant baseUrl err
        fetchErr (Right _) = error "Success response"
    fetchErr resE `shouldBe` Error.TooManyRequests (Just $ fromIntegral secs)

startServer :: IO (Word -> Word -> SC.ClientM (), BaseUrl)
startServer = do
    let (server, client, baseUrl) = Server.retryAfter cfg
    _ <- liftIO $ Conc.forkIO server
    return (client, baseUrl)
  where
    cfg = Server.Config
        { cfgPort = 12345
        }
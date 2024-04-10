import Control.Concurrent (
    MVar,
    newEmptyMVar,
    takeMVar, threadDelay,
 )
import Control.Monad (when)
import Data.Aeson
import Data.Either
import Data.Text (pack)
import Matcher.API.Client
import Matcher.API.Event (APIEvent (..))
import Network.AMQP
import System.Environment (getEnv)
import System.Exit (exitFailure)
import GHC.Base (maxInt)

main :: IO ()
main = do
    apiUrl <- getEnv "API_URL"
    apiKey <- getEnv "MATCHER_API_KEY"
    rabbitHost <- getEnv "RABBIT_HOST"
    rabbitUname <- pack <$> getEnv "RABBIT_USER"
    rabbitPass <- pack <$> getEnv "RABBIT_PASS"
    pingRes <- ping (newAPIClient apiUrl apiKey)
    when (isLeft pingRes) (print pingRes >> exitFailure)
    conn <- openConnection rabbitHost "/" rabbitUname rabbitPass
    ch <- openChannel conn
    m <- newEmptyMVar :: IO (MVar ())

    putStrLn "Matcher ready to match! Waiting for messages..."
    _ <- consumeMsgs ch "api" NoAck deliveryHandler
    threadDelay maxInt
    closeConnection conn

deliveryHandler :: (Message, Envelope) -> IO ()
deliveryHandler (msg, _) = case decode (msgBody msg) :: Maybe APIEvent of
    Nothing -> putStrLn "Could not parse event"
    Just a -> putStrLn $ "Received Event " <> show (actionType a)

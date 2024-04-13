import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Aeson
import Data.Either
import Data.Text (pack)
import GHC.Base (maxInt)
import Matcher.API.Client
import Matcher.API.Event (APIEvent (..))
import Network.AMQP
import System.Environment (getEnv)
import System.Exit (exitFailure)

main :: IO ()
main = do
    apiUrl <- getEnv "API_URL"
    apiKey <- getEnv "MATCHER_API_KEY"
    rabbitHost <- getEnv "RABBIT_HOST"
    rabbitUname <- pack <$> getEnv "RABBIT_USER"
    rabbitPass <- pack <$> getEnv "RABBIT_PASS"
    pingRes <- ping (APIClient {..})
    when (isLeft pingRes) (print pingRes >> exitFailure)
    conn <- openConnection rabbitHost "/" rabbitUname rabbitPass
    ch <- openChannel conn

    putStrLn "Matcher ready to match! Waiting for messages..."
    _ <- consumeMsgs ch "api" NoAck deliveryHandler
    threadDelay maxInt
    closeConnection conn

deliveryHandler :: (Message, Envelope) -> IO ()
deliveryHandler (msg, _) = case decode (msgBody msg) :: Maybe APIEvent of
    Nothing -> putStrLn "Could not parse event"
    Just a -> putStrLn $ "Received Event " <> show (actionType a)

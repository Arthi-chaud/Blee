import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Aeson
import Data.Either
import Data.Text (pack)
import GHC.Base (maxInt)
import Matcher (handleAPIEvent)
import Matcher.API.Client
import Matcher.API.Event (APIEvent (..))
import Matcher.TMDB.Client (TMDBClient (TMDBClient))
import Network.AMQP
import System.Environment (getEnv)
import System.Exit (exitFailure)

main :: IO ()
main = do
    apiUrl <- getEnv "API_URL"
    tmdbApiKey <- getEnv "TMDB_API_KEY"
    apiKey <- getEnv "MATCHER_API_KEY"
    rabbitHost <- getEnv "RABBIT_HOST"
    rabbitUname <- pack <$> getEnv "RABBIT_USER"
    rabbitPass <- pack <$> getEnv "RABBIT_PASS"
    let apiClient = APIClient {..}
        tmdbClient = TMDBClient tmdbApiKey
    pingRes <- ping apiClient
    when (isLeft pingRes) (print pingRes >> exitFailure)
    conn <- openConnection rabbitHost "/" rabbitUname rabbitPass
    ch <- openChannel conn

    putStrLn "Matcher ready to match! Waiting for messages..."
    _ <- consumeMsgs ch "api" NoAck (deliveryHandler apiClient tmdbClient)
    threadDelay maxInt
    closeConnection conn

deliveryHandler :: APIClient -> TMDBClient -> (Message, Envelope) -> IO ()
deliveryHandler client tmdbClient (msg, _) =
    let body = msgBody msg
    in case decode body :: Maybe APIEvent of
        Nothing -> putStrLn "Could not parse event: " >> print body
        Just event -> do
            res <- handleAPIEvent client tmdbClient event
            case res of
                Left err -> putStrLn err
                Right _ -> return ()

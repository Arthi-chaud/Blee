import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (pack)
import Matcher.API.Event (APIEvent (..))
import Network.AMQP
import System.Environment (getEnv)

main :: IO ()
main = do
    rabbitHost <- getEnv "RABBIT_HOST"
    rabbitUname <- pack <$> getEnv "RABBIT_USER"
    rabbitPass <- pack <$> getEnv "RABBIT_PASS"
    conn <- openConnection rabbitHost "/" rabbitUname rabbitPass
    ch <- openChannel conn

    putStrLn " [*] Waiting for messages. To exit press CTRL+C"
    _ <- consumeMsgs ch "api" NoAck deliveryHandler
    _ <- wait
    closeConnection conn

deliveryHandler :: (Message, Envelope) -> IO ()
deliveryHandler (msg, _) = case decode (msgBody msg) :: Maybe APIEvent of
    Nothing -> putStrLn "Could not parse event"
    Just a -> putStrLn $ " [x] Received Event " <> show (actionType a)

wait :: IO ()
wait = do
    putStr ""
    wait
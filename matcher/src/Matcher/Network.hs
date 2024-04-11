module Matcher.Network (request) where

import Data.Bifunctor (Bifunctor (second))
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Char8 (pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Version (showVersion)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (HeaderName, Status (Status, statusCode))
import Paths_matcher (version)
import Text.Printf (printf)

request ::
    String
    -- ^ URL
    -> [(HeaderName, String)]
    -- ^ Headers
    -> [(String, String)]
    -- ^ Query Parameters
    -> IO (Either String ByteString)
request url headers query = do
    manager <- newManager tlsManagerSettings
    r <-
        setQueryString ((\(a, b) -> (pack a, Just $ pack b)) <$> query)
            <$> parseRequest url
    let h =
            requestHeaders r
                <> [   ( "User-Agent",
                         pack $
                            printf
                                "Matcha/%s +https://github.com/Arthi-chaud/Blee/matcher"
                                (showVersion version)
                       )
                   ]
                <> (second pack <$> headers)
    response <- httpLbs r {requestHeaders = h} manager
    case responseStatus response of
        Status 200 _ -> return $ Right (toStrict $ responseBody response)
        status ->
            return $
                Left
                    ( printf
                        "Expected an OK Status code, got %d: '%s'"
                        (statusCode status)
                        (decodeUtf8 $ toStrict $ responseBody response)
                    )

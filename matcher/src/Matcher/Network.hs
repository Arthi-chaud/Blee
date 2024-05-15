module Matcher.Network (request, post, put) where

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

post ::
    String
    -- ^ URL
    -> String
    -- ^ Mime
    -> [(HeaderName, String)]
    -- ^ Headers
    -> ByteString
    -- ByteString Body
    -> IO (Either String ByteString)
post url mime headers body = do
    manager <- newManager tlsManagerSettings
    r <- parseRequest url
    let h =
            requestHeaders r
                <> [   ( "User-Agent",
                         pack $
                            printf
                                "Matcha/%s +https://github.com/Arthi-chaud/Blee/matcher"
                                (showVersion version)
                       ),
                     ("Content-Type", pack mime <> "; charset=utf-8")
                   ]
                <> (second pack <$> headers)
    response <-
        httpLbs
            r {requestBody = RequestBodyBS body, method = "POST"}
                { requestHeaders = h
                }
            manager
    case responseStatus response of
        Status 201 _ -> return $ Right (toStrict $ responseBody response)
        status ->
            return $
                Left
                    ( printf
                        "Expected an OK Status code, got %d: '%s'"
                        (statusCode status)
                        (decodeUtf8 $ toStrict $ responseBody response)
                    )

put ::
    String
    -- ^ URL
    -> [(HeaderName, String)]
    -- ^ Headers
    -> ByteString
    -- ByteString Body
    -> IO (Either String ByteString)
put url headers body = do
    manager <- newManager tlsManagerSettings
    r <- parseRequest url
    let h =
            requestHeaders r
                <> [   ( "User-Agent",
                         pack $
                            printf
                                "Matcha/%s +https://github.com/Arthi-chaud/Blee/matcher"
                                (showVersion version)
                       ),
                     ("Content-Type", "application/json" <> "; charset=utf-8")
                   ]
                <> (second pack <$> headers)
    response <-
        httpLbs
            r {requestBody = RequestBodyBS body, method = "PUT"}
                { requestHeaders = h
                }
            manager
    case responseStatus response of
        Status 204 _ -> return $ Right (toStrict $ responseBody response)
        status ->
            return $
                Left
                    ( printf
                        "Expected an OK Status code, got %d: '%s'"
                        (statusCode status)
                        (decodeUtf8 $ toStrict $ responseBody response)
                    )

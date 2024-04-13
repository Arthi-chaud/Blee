module Matcher.API.Client (APIClient (..), ping, pushArtistExternalId, pushArtistPoster) where

import Data.Aeson (encode)
import Data.ByteString
import Matcher.API.Dto (ArtistExternalId)
import Matcher.Network

data APIClient = APIClient
    { apiUrl :: String,
      apiKey :: String
    }

apiRequest ::
    APIClient
    -> String
    -- ^ Route
    -> [(String, String)]
    -- ^ Query Parameters
    -> IO (Either String ByteString)
apiRequest (APIClient url key) route = request (url <> route) [("x-api-key", key)]

apiPost ::
    APIClient
    -> String
    -- ^ Route
    -> ByteString
    -- ^ Body
    -> IO (Either String ByteString)
apiPost (APIClient url key) route = post (url <> route) "application/json" [("x-api-key", key)]

apiPostBinary ::
    APIClient
    -> String
    -- ^ Route
    -> ByteString
    -- ^ Body
    -> IO (Either String ByteString)
apiPostBinary (APIClient url key) route = post (url <> route) "application/octet-stream" [("x-api-key", key)]

-- | Returns () if the ping succeeded
ping :: APIClient -> IO (Either String ())
ping client = (() <$) <$> apiRequest client "/" []

pushArtistExternalId :: APIClient -> ArtistExternalId -> IO (Either String ())
pushArtistExternalId client dto = (() <$) <$> apiPost client "/external_ids" (toStrict $ encode dto)

pushArtistPoster :: APIClient -> String -> ByteString -> IO (Either String ())
pushArtistPoster client uuid posterBytes =
    (() <$) <$> apiPostBinary client ("/artists/" ++ uuid ++ "/poster") posterBytes

module Matcher.API.Client (
    APIClient (..),
    ping,
    pushArtistExternalId,
    pushPackageExternalId,
    pushArtistPoster,
    pushPackagePoster,
    getArtist,
    getPackage,
    updatePackage,
) where

import Data.Aeson (eitherDecodeStrict', encode)
import Data.ByteString
import Matcher.API.Dto (
    Artist,
    ArtistExternalId,
    Package,
    PackageExternalId,
    UpdatePackage (..),
 )
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

apiPut ::
    APIClient
    -> String
    -- ^ Route
    -> ByteString
    -- ^ Body
    -> IO (Either String ByteString)
apiPut (APIClient url key) route = put (url <> route) [("x-api-key", key)]

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

getArtist :: APIClient -> String -> IO (Either String Artist)
getArtist client uuid = (eitherDecodeStrict' =<<) <$> apiRequest client ("/artists/" ++ uuid) []

getPackage :: APIClient -> String -> IO (Either String Package)
getPackage client uuid = (eitherDecodeStrict' =<<) <$> apiRequest client ("/packages/" ++ uuid) []

pushPackageExternalId :: APIClient -> PackageExternalId -> IO (Either String ())
pushPackageExternalId client dto = (() <$) <$> apiPost client "/external_ids" (toStrict $ encode dto)

updatePackage :: APIClient -> String -> UpdatePackage -> IO (Either String ())
updatePackage client uuid dto = (() <$) <$> apiPut client ("/packages/" ++ uuid) (toStrict $ encode dto)

pushPackagePoster :: APIClient -> String -> ByteString -> IO (Either String ())
pushPackagePoster client uuid posterBytes =
    (() <$) <$> apiPostBinary client ("/packages/" ++ uuid ++ "/poster") posterBytes

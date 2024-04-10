module Matcher.API.Client (APIClient, newAPIClient, ping) where

import Data.ByteString
import Matcher.Network

data APIClient = APIClient
    { apiUrl :: String,
      apiKey :: String
    }

newAPIClient :: String -> String -> APIClient
newAPIClient = APIClient

apiRequest ::
    APIClient
    -> String
    -- ^ Route
    -> [(String, String)]
    -- ^ Query Parameters
    -> IO (Either String ByteString)
apiRequest (APIClient url key) route = request (url <> route) [("x-api-key", key)]

-- | Returns () if the ping succeeded
ping :: APIClient -> IO (Either String ())
ping client = (() <$) <$> apiRequest client "/" []

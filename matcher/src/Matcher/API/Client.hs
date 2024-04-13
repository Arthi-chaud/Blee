module Matcher.API.Client (APIClient(..), ping) where

import Data.ByteString
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

-- | Returns () if the ping succeeded
ping :: APIClient -> IO (Either String ())
ping client = (() <$) <$> apiRequest client "/" []

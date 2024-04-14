module Matcher.API.Dto.BaseExternalId (BaseExternalId (..), jsonPairs) where

import Data.Aeson.Types

data BaseExternalId = BaseExternalId
    { url :: String,
      value :: String,
      description :: Maybe String,
      rating :: Maybe Int,
      providerName :: String
    }

jsonPairs :: BaseExternalId -> [Pair]
jsonPairs i =
    [ "url" .= url i,
      "value" .= value i,
      "description" .= description i,
      "rating" .= rating i,
      "provider_name" .= providerName i
    ]

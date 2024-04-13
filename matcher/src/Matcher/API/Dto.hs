module Matcher.API.Dto (ArtistExternalId (..), BaseExternalId (..)) where

import Data.Aeson.Types

data BaseExternalId = ExternalId
    { url :: String,
      value :: String,
      description :: Maybe String,
      rating :: Int,
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

data ArtistExternalId = ArtistExternalId
    { artistId :: String,
      externalId :: BaseExternalId
    }

instance ToJSON ArtistExternalId where
    toJSON (ArtistExternalId i d) = object $ ("artist_id" .= i) : jsonPairs d

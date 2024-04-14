module Matcher.API.Dto.Artist (ArtistExternalId (..), Artist (..)) where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Matcher.API.Dto.BaseExternalId

data ArtistExternalId = ArtistExternalId
    { artistId :: String,
      artistExternalId :: BaseExternalId
    }

instance ToJSON ArtistExternalId where
    toJSON (ArtistExternalId i d) = object $ ("artist_id" .= i) : jsonPairs d

data Artist = Artist
    { id :: String
    }
    deriving (Generic, Show)

instance FromJSON Artist

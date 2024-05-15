module Matcher.API.Dto.Package (Package (..), PackageExternalId (..), UpdatePackage (..)) where

import Data.Aeson.Types
import Data.Time.Calendar
import GHC.Generics (Generic)
import Matcher.API.Dto.BaseExternalId

data Package = Package
    { artist_name :: Maybe String,
      release_year :: Maybe Day,
      poster_id :: Maybe String
    }
    deriving (Generic, Show)

instance FromJSON Package where
    parseJSON = withObject "API Package" $ \v ->
        Package
            <$> v .: "artist_name"
            <*> v .: "release_year"
            <*> v .: "poster_id"

data PackageExternalId = PackageExternalId
    { packageId :: String,
      packageExternalId :: BaseExternalId
    }

instance ToJSON PackageExternalId where
    toJSON (PackageExternalId i d) = object $ ("package_id" .= i) : jsonPairs d

data UpdatePackage = UpdatePackage
    {releaseDate :: Day}

instance ToJSON UpdatePackage where
    toJSON (UpdatePackage d) = object ["release_date" .= d]

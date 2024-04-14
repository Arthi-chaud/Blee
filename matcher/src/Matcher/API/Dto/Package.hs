module Matcher.API.Dto.Package (Package (..), PackageExternalId (..)) where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Matcher.API.Dto.BaseExternalId

data Package = Package
    { artist_name :: Maybe String,
      poster_id :: Maybe String
    }
    deriving (Generic, Show)

instance FromJSON Package

data PackageExternalId = PackageExternalId
    { packageId :: String,
      packageExternalId :: BaseExternalId
    }

instance ToJSON PackageExternalId where
    toJSON (PackageExternalId i d) = object $ ("package_id" .= i) : jsonPairs d

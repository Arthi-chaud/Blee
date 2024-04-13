module Matcher.TMDB.Models (Page (..), ArtistSearchResult (..), ArtistDetails (..)) where

import Data.Aeson

data ArtistSearchResult = ArtistSearchResult
    { identifier :: Integer,
      name :: String,
      originalName :: String,
      profilePath :: Maybe String
    }

data ArtistDetails = ArtistDetails
    { biography :: Maybe String
    }

instance FromJSON ArtistDetails where
    parseJSON = withObject "TMDB Artist Details" $ \v ->
        ArtistDetails
            <$> v .: "biography"

newtype Page a = Page
    { results :: [a]
    }

instance (FromJSON a) => FromJSON (Page a) where
    parseJSON = withObject "Page" $ \v -> Page <$> v .: "results"

instance FromJSON ArtistSearchResult where
    parseJSON = withObject "TMDB Artist" $ \v ->
        ArtistSearchResult
            <$> v .: "id"
            <*> v .: "name"
            <*> v .: "original_name"
            <*> v .: "profile_path"

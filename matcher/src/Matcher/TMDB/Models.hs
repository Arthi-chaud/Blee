module Matcher.TMDB.Models (
    Page (..),
    ArtistSearchResult (..),
    ArtistDetails (..),
    MovieSearchResult (..),
    MovieDetails (..),
) where

import Data.Aeson

data ArtistSearchResult = ArtistSearchResult
    { identifier :: Integer,
      name :: String,
      originalName :: String,
      profilePath :: Maybe String
    }

instance FromJSON ArtistSearchResult where
    parseJSON = withObject "TMDB Artist" $ \v ->
        ArtistSearchResult
            <$> v .: "id"
            <*> v .: "name"
            <*> v .: "original_name"
            <*> v .: "profile_path"

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

data MovieSearchResult = MovieSearchResult
    { i :: Integer,
      title :: String,
      vote_average :: Maybe Double,
      posterPath :: Maybe String
    }

data MovieDetails = MovieDetails
    { overview :: Maybe String
    }

instance FromJSON MovieSearchResult where
    parseJSON = withObject "TMDB Movie" $ \v ->
        MovieSearchResult
            <$> v .: "id"
            <*> v .: "title"
            <*> v .: "vote_average"
            <*> v .: "poster_path"

instance FromJSON MovieDetails where
    parseJSON = withObject "TMDB Movie Details" $ \v ->
        MovieDetails
            <$> v .: "overview"

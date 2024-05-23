module Matcher.TMDB.Models (
    Page (..),
    ArtistSearchResult (..),
    ArtistDetails (..),
    MovieSearchResult (..),
    MovieDetails (..),
    MovieImages (..),
    MovieImage (..),
) where

import Data.Aeson
import Data.Functor
import Data.Time.Calendar
import GHC.Generics (Generic)

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
            <$> ( v .: "biography" <&> \case
                    Just "" -> Nothing
                    x -> x
                )

newtype Page a = Page
    { results :: [a]
    }

instance (FromJSON a) => FromJSON (Page a) where
    parseJSON = withObject "Page" $ \v -> Page <$> v .: "results"

data MovieSearchResult = MovieSearchResult
    { i :: Integer,
      title :: String,
      voteAverage :: Maybe Double,
      releaseDate :: Maybe Day
    }

data MovieImages = MovieImages
    { backdrops :: [MovieImage],
      posters :: [MovieImage]
    }
    deriving (Generic)

instance FromJSON MovieImages

data MovieImage = MovieImage
    { file_path :: String
    }
    deriving (Generic)

instance FromJSON MovieImage

data MovieDetails = MovieDetails
    { overview :: Maybe String,
      images :: MovieImages
    }

instance FromJSON MovieSearchResult where
    parseJSON = withObject "TMDB Movie" $ \v ->
        MovieSearchResult
            <$> v .: "id"
            <*> v .: "title"
            <*> ( v .: "vote_average" <&> \case
                    Just 0 -> Nothing
                    x -> x
                )
            <*> v .: "release_date"

instance FromJSON MovieDetails where
    parseJSON = withObject "TMDB Movie Details" $ \v ->
        MovieDetails
            <$> ( v .: "overview" <&> \case
                    Just "" -> Nothing
                    x -> x
                )
            <*> v .: "images"

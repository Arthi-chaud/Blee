module Matcher.TMDB.Client (
    TMDBClient (..),
    searchArtist,
    getArtistDetails,
    downloadImage,
    searchMovie,
    getMovieDetails,
) where

import Data.Aeson (eitherDecodeStrict')
import Data.ByteString (ByteString)
import Data.Text
import Data.Text.Metrics (levenshtein)
import Matcher.Network
import Matcher.TMDB.Models
import Text.Slugify (slugify)

newtype TMDBClient = TMDBClient
    { apiKey :: String
    }

tmdbRequest ::
    TMDBClient
    -> String
    -- ^ Route
    -> [(String, String)]
    -- ^ Query Parameters
    -> IO (Either String ByteString)
tmdbRequest (TMDBClient key) route query =
    request
        ("https://api.themoviedb.org/3" <> route)
        []
        (query <> [("api_key", key)])

searchArtist :: TMDBClient -> String -> IO (Either String ArtistSearchResult)
searchArtist client token = do
    searchRawRes <-
        tmdbRequest
            client
            "/search/person"
            [ ("query", token),
              ("page", "1"),
              ("include_adult", "false"),
              ("language", "en-US")
            ]
    return $
        searchRawRes
            >>= ( \s -> do
                    page <- eitherDecodeStrict' s :: Either String (Page ArtistSearchResult)
                    case results page of
                        [] -> Left "Empty Page"
                        (a : _) ->
                            if levenshtein (slugify $ pack $ name a) (slugify $ pack token) > 2
                                then Left "No Match"
                                else
                                    return
                                        a
                                            { profilePath = ("https://image.tmdb.org/t/p/original" ++) <$> profilePath a
                                            }
                )

searchMovie :: TMDBClient -> String -> IO (Either String MovieSearchResult)
searchMovie client token = do
    searchRawRes <-
        tmdbRequest
            client
            "/search/movie"
            [ ("query", token),
              ("page", "1"),
              ("include_adult", "false"),
              ("language", "en-US")
            ]
    return $
        searchRawRes
            >>= ( \s -> do
                    page <- eitherDecodeStrict' s :: Either String (Page MovieSearchResult)
                    case results page of
                        [] -> Left "Empty Page"
                        (a : _) -> Right a
                )

getArtistDetails :: TMDBClient -> Integer -> IO (Either String ArtistDetails)
getArtistDetails client artistId = do
    rawRes <-
        tmdbRequest
            client
            ("/person/" <> show artistId)
            []
    return $
        rawRes
            >>= eitherDecodeStrict'

getMovieDetails :: TMDBClient -> Integer -> IO (Either String MovieDetails)
getMovieDetails client movieId = do
    rawRes <-
        tmdbRequest
            client
            ("/movie/" <> show movieId)
            [("append_to_response", "images")]
    return $
        rawRes
            >>= \s -> do
                details <- eitherDecodeStrict' s
                return
                    details
                        { images = buildFullImagesPath $ images details
                        }
    where
        buildFullImagesPath images =
            images
                { backdrops =
                    buildImagePath
                        <$> backdrops images,
                  posters =
                    buildImagePath
                        <$> posters images
                }
        buildImagePath image =
            image {file_path = "https://image.tmdb.org/t/p/original" ++ file_path image}

downloadImage :: TMDBClient -> String -> IO (Either String ByteString)
downloadImage client imageUrl =
    request
        imageUrl
        []
        [("api_key", apiKey client)]

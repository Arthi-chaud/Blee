module Matcher.TMDB.Client (TMDBClient (..), searchArtist, getArtistDetails) where

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
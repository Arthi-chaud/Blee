module Matcher (handleAPIEvent) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.Except
import Data.Functor (void)
import Data.List.Extra ((!?))
import Data.Maybe
import Data.Time
import Matcher.API.Client
import Matcher.API.Dto
import Matcher.API.Event
import Matcher.TMDB.Client
import Matcher.TMDB.Models (
    ArtistDetails (..),
    ArtistSearchResult (..),
    MovieDetails (..),
    MovieImage (file_path),
    MovieImages (backdrops, posters),
    MovieSearchResult (..),
 )
import Prelude hiding (id)

handleAPIEvent :: APIClient -> TMDBClient -> APIEvent -> IO (Either String ())
handleAPIEvent client tmdb (APIEvent "artist" Insert uuid name) = runExceptT $ do
    artist <- ExceptT $ searchArtist tmdb name
    ArtistDetails artistDescription <-
        ExceptT $ getArtistDetails tmdb (identifier artist)
    let artistDto =
            ArtistExternalId
                uuid
                BaseExternalId
                    { url = "https://www.themoviedb.org/person/" ++ show (identifier artist),
                      value = show $ identifier artist,
                      description = artistDescription,
                      rating = Nothing,
                      providerName = "TMDB"
                    }
    _ <- ExceptT $ pushArtistExternalId client artistDto
    case profilePath artist of
        Nothing -> return ()
        Just posterUrl -> do
            posterBytes <- ExceptT $ downloadImage tmdb posterUrl
            ExceptT $ pushArtistPoster client uuid posterBytes
handleAPIEvent client tmdb (APIEvent "package" Insert uuid name) = runExceptT $ do
    package <- ExceptT $ getPackage client uuid
    let packageSearchToken = case artist_name package of
            Nothing -> name
            Just artistName -> artistName ++ " " ++ name
    movie <- ExceptT $ searchMovie tmdb packageSearchToken
    movieDetails <-
        ExceptT $ getMovieDetails tmdb (i movie)
    let packageDto =
            PackageExternalId
                uuid
                BaseExternalId
                    { url = "https://www.themoviedb.org/movie/" ++ show (i movie),
                      value = show $ i movie,
                      description = overview movieDetails,
                      rating = (\r -> round (10 * r) :: Int) <$> voteAverage movie,
                      providerName = "TMDB"
                    }
    _ <- ExceptT $ pushPackageExternalId client packageDto
    _ <- ExceptT $ case Matcher.TMDB.Models.releaseDate movie of
        Nothing -> return $ Right ()
        Just date -> do
            when
                (shouldUpdatePackageDate date (release_year package))
                (void $ updatePackage client uuid (UpdatePackage date))
            return $ Right ()
    forM_
        [ (posters, poster_id, pushPackagePoster),
          (backdrops, banner_id, pushPackageBanner)
        ]
        $ \(imageType, column, apiCall) -> case imageType (images movieDetails) !? 0 of
            Nothing -> return ()
            Just image -> when (isNothing (column package)) $ do
                imageBytes <- ExceptT $ downloadImage tmdb (file_path image)
                ExceptT $ apiCall client uuid imageBytes
    where
        shouldUpdatePackageDate _ Nothing = True
        shouldUpdatePackageDate (YearMonthDay tmdbYear _ _) (Just (YearMonthDay apiYear _ _)) = tmdbYear == apiYear
handleAPIEvent _ _ _ = return $ Left "No handler for this event"

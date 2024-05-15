module Matcher (handleAPIEvent) where

import Control.Monad (when)
import Control.Monad.Trans.Except
import Data.Functor (void)
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
            posterBytes <- ExceptT $ getPoster tmdb posterUrl
            ExceptT $ pushArtistPoster client uuid posterBytes
handleAPIEvent client tmdb (APIEvent "package" Insert uuid name) = runExceptT $ do
    package <- ExceptT $ getPackage client uuid
    let packageSearchToken = case artist_name package of
            Nothing -> name
            Just artistName -> artistName ++ " " ++ name
    movie <- ExceptT $ searchMovie tmdb packageSearchToken
    MovieDetails packageDescription <-
        ExceptT $ getMovieDetails tmdb (i movie)
    let packageDto =
            PackageExternalId
                uuid
                BaseExternalId
                    { url = "https://www.themoviedb.org/movie/" ++ show (i movie),
                      value = show $ i movie,
                      description = packageDescription,
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

    case posterPath movie of
        Nothing -> return ()
        Just posterUrl -> when (isNothing (poster_id package)) $ do
            posterBytes <- ExceptT $ getPoster tmdb posterUrl
            ExceptT $ pushPackagePoster client uuid posterBytes
    where
        shouldUpdatePackageDate _ Nothing = True
        shouldUpdatePackageDate (YearMonthDay tmdbYear _ _) (Just (YearMonthDay apiYear _ _)) = tmdbYear == apiYear
handleAPIEvent _ _ _ = return $ Left "No handler for this event"

module Matcher (handleAPIEvent) where

import Control.Monad.Trans.Except
import Matcher.API.Client
import Matcher.API.Dto
import Matcher.API.Event
import Matcher.TMDB.Client
import Matcher.TMDB.Models (
    ArtistDetails (ArtistDetails),
    ArtistSearchResult (..),
 )
import Prelude hiding (id)

handleAPIEvent :: APIClient -> TMDBClient -> APIEvent -> IO (Either String ())
handleAPIEvent client tmdb (APIEvent "artist" Insert uuid name) = runExceptT $ do
    artist <- ExceptT $ searchArtist tmdb name
    ArtistDetails artistDescription <-
        ExceptT $ getArtistDetails tmdb (identifier artist)
    let artistDto =
            ArtistExternalId
                { artistId = uuid,
                  externalId =
                    BaseExternalId
                        { url = "https://www.themoviedb.org/person/" ++ show (identifier artist),
                          value = show $ identifier artist,
                          description = artistDescription,
                          rating = Nothing,
                          providerName = "TMDB"
                        }
                }
    ExceptT $
        pushArtistExternalId client artistDto
            >> case profilePath artist of
                Nothing -> return $ Right ()
                Just posterUrl -> runExceptT $ do
                    posterBytes <- ExceptT $ getPoster tmdb posterUrl
                    ExceptT $ pushArtistPoster client uuid posterBytes
handleAPIEvent _ _ _ = return $ Left "No handler for this event"

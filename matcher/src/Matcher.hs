module Matcher (handleAPIEvent) where

import Control.Monad.Trans.Except
import Matcher.API.Client
import Matcher.API.Dto
import Matcher.API.Event
import Matcher.TMDB.Client
import Matcher.TMDB.Models (
    ArtistDetails (ArtistDetails),
    ArtistSearchResult (identifier),
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
    ExceptT $ pushArtistExternalId client artistDto
handleAPIEvent _ _ _ = return $ Left "No handler for this event"

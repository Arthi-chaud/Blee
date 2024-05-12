module Matcher.TestTMDB (specs) where

import Data.Maybe (fromJust)
import LoadEnv (loadEnv)
import Matcher.TMDB.Client
import Matcher.TMDB.Models
import System.Environment (lookupEnv)
import Test.Hspec

specs :: Spec
specs = describe "TMDB" $ do
    _ <- runIO loadEnv
    apiKey <- runIO $ lookupEnv "TMDB_API_KEY"
    let tmdbClient = TMDBClient (fromJust apiKey)
    describe "Search Artist" $ do
        it "Should Search and find Artist" $ do
            searchArtist tmdbClient "Madonna"
                >>= ( \case
                        Left e -> expectationFailure e
                        Right res -> do
                            identifier res `shouldBe` 3125
                            name res `shouldBe` "Madonna"
                            originalName res `shouldBe` "Madonna"
                            profilePath res
                                `shouldBe` Just "https://image.tmdb.org/t/p/original/8XtGxpB4z428QDgwKlFYPktYHFC.jpg"
                    )
        it "Should Fail to find band" $ do
            searchArtist tmdbClient "Garbage"
                >>= ( \case
                        Left _ -> return ()
                        Right r -> expectationFailure $ "Should have failed to get band. Got: " ++ name r
                    )
    describe "Get Artist Details" $ do
        it "Should Get Artist Details" $ do
            getArtistDetails tmdbClient 3125
                >>= ( \case
                        Left e -> expectationFailure e
                        Right (ArtistDetails Nothing) -> expectationFailure "No biography found"
                        Right (ArtistDetails (Just description)) -> do
                            take 10 description `shouldBe` "Madonna (b"
                            reverse (take 10 $ reverse description) `shouldBe` "le artist."
                    )
        it "Should Get Null Description, not empty string" $ do
            getArtistDetails tmdbClient 3800537
                >>= ( \case
                        Left e -> expectationFailure e
                        Right (ArtistDetails details) -> details `shouldBe` Nothing
                    )
    describe "Search Package" $ do
        it "Should Get Package" $ do
            searchMovie tmdbClient "The Corrs - Live at Lansdowne Road"
                >>= ( \case
                        Left e -> expectationFailure e
                        Right res -> do
                            i res `shouldBe` 2188
                            title res `shouldBe` "The Corrs: Live at Lansdowne Road"
                            vote_average res `shouldBe` (Just 7.7)
                            posterPath res
                                `shouldBe` Just "https://image.tmdb.org/t/p/original/ApXQQS8peDN9wzXhpU30xzFH5TN.jpg"
                    )
    describe "Get Package Details" $ do
        it "Should Get Package Details" $ do
            getMovieDetails tmdbClient 2188
                >>= ( \case
                        Left e -> expectationFailure e
                        Right (MovieDetails Nothing) -> expectationFailure "No description found"
                        Right (MovieDetails (Just description)) -> do
                            take 10 description `shouldBe` "Irish-Celt"
                            reverse (take 10 $ reverse description) `shouldBe` "by fields."
                    )
        it "Should Get Null Description, not empty string" $ do
            getMovieDetails tmdbClient 60516
                >>= ( \case
                        Left e -> expectationFailure e
                        Right (MovieDetails details) -> details `shouldBe` Nothing
                    )

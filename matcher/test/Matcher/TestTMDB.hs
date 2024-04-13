module Matcher.TestTMDB (specs) where
import Test.Hspec
import LoadEnv (loadEnv)
import System.Environment (lookupEnv)
import Matcher.TMDB.Client
import Matcher.TMDB.Models
import Data.Maybe (fromJust)

specs :: Spec
specs = describe "TMDB" $ do
    _ <- runIO loadEnv
    apiKey <- runIO $ lookupEnv "TMDB_API_KEY"
    let tmdbClient = TMDBClient (fromJust apiKey)
    describe "Search Artist" $ do
        it "Should Search and find Artist" $ do
            searchArtist tmdbClient "Madonna"
                >>= (\case
                        Left e -> expectationFailure e
                        Right res -> do
                            Matcher.TMDB.Models.id res `shouldBe` 3125
                            name res `shouldBe` "Madonna"
                            originalName res `shouldBe` "Madonna"
                            profilePath res `shouldBe` Just "https://image.tmdb.org/t/p/original/pI6g1iVlUy7cUAZ6AspVXWq4kli.jpg"
                    )
        it "Should Fail to find band" $ do
            searchArtist tmdbClient "Garbage"
                >>= (\case
                        Left _ -> return ()
                        Right r -> expectationFailure $ "Should have failed to get band. Got: " ++ name r
                    )
    describe "Get Artist Details" $ do
        it "Should Get Artist Details" $ do
            getArtistDetails tmdbClient 3125
                >>= (\case
                        Left e -> expectationFailure e
                        Right (ArtistDetails Nothing) -> expectationFailure "No biography found"
                        Right (ArtistDetails (Just description)) -> do
                            take 10 description `shouldBe` "Madonna (b"
                            reverse (take 10 $ reverse description) `shouldBe` " industry."
                    )
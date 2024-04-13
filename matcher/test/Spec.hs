import qualified Matcher.TestTMDB
import Test.Hspec

main :: IO ()
main = hspec $ do
    Matcher.TestTMDB.specs

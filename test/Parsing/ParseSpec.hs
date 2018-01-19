module Parsing.ParseSpec (spec) where
import System.FilePath (combine)
import Control.Monad (forM_)
import Data.Either (isRight)
import Test.Hspec
import Parsing (parse)
import Errors.LatteError


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "parse" $ do
        forM_ testCases $ \(fname, pred, behaves) ->
            it behaves $ do
                source <- readFile $ combine fixturesDir fname
                parse source `shouldSatisfy` pred


testCases :: [(FilePath, (Either a b -> Bool), String)]
testCases = [
    ("bad001.lat", not.ok, "fails on unclosed comment"),
    ("bad002.lat", not.ok, "fails on toplevel variable"),
    ("bad004.lat", not.ok, "fails on unmatched parens"),
    ("bad005.lat", not.ok, "fails on invalid function declaration"),
    ("counter.lat", ok, "succeeds on class definition"),
    ("list.lat", ok, "succeeds with structs"),
    ("linked.lat", ok, "succeeds on class definition with methods and nulls"),
    ("points.lat", ok, "succeeds on class definition with inheritance"),
    ("array001.lat", ok, "succeeds on int array (declaration and setting of elements)"),
    ("array002.lat", ok, "succeeds on advanced int array usage")]


ok :: Either a b -> Bool
ok = isRight


fixturesDir :: FilePath
fixturesDir = combine "test" "fixtures"

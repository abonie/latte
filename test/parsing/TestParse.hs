import Test.HUnit
import System.Directory
import System.FilePath
import Control.Monad (when)
import Data.Either
import AbsLatte
import ParLatte (pProgram)
import LexLatte (tokens)
import ErrM (Err(..))


data Result = Yes | No

testFiles :: [(String, Result)]
testFiles = [
    ("bad001.lat", No),
    ("bad002.lat", No)]


main = do
    args <- mapM readTest testFiles
    runTestTT $ TestList $ map makeTest args


readTest :: (String, Result) -> IO (Err PProgram, String, Result)
readTest (filename, res) = do
    f <- readFile filename
    return (pProgram $ tokens f, filename, res)


makeTest :: (Err PProgram, String, Result) -> Test
makeTest (Ok _, _, Yes) = TestCase $ assertBool "" True  -- TODO

makeTest (Bad _, _, No) = TestCase $ assertBool "" True  -- TODO

makeTest (Ok _, label, No) = TestLabel label $ TestCase $
    assertFailure "Unexpected parse success"

makeTest (Bad err, label, Yes) = TestLabel label $ TestCase $
    assertFailure $ "Error while parsing: " ++ (show err)

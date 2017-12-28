import Test.HUnit
import System.Directory
import System.FilePath
import Control.Monad (when)
import Data.Either
import AbsLatte
import ParLatte (pProgram)
import LexLatte (tokens)
import ErrM (Err(..))


testFiles :: [(String, Bool)]
testFiles = [
    ("bad001.lat", False),
    ("bad004.lat", False),
    ("bad005.lat", False),
    ("bad002.lat", False),
    ("counter.lat", True),
    ("linked.lat", True),
    ("points.lat", True),
    ("queue.lat", True),
    ("array002.lat", True),
    ("array001.lat", True)]


main = do
    args <- mapM readTest testFiles
    runTestTT $ TestList $ map makeTest args


readTest :: (String, Bool) -> IO (Err PProgram, String, Bool)
readTest (filename, res) = do
    f <- readFile filename
    return (pProgram $ tokens f, filename, res)


makeTest :: (Err PProgram, String, Bool) -> Test
makeTest (Ok _, _, True) = TestCase $ assertBool "" True  -- TODO

makeTest (Bad _, _, False) = TestCase $ assertBool "" True  -- TODO

makeTest (Ok _, label, False) = TestLabel label $ TestCase $
    assertFailure "Unexpected parse success"

makeTest (Bad err, label, True) = TestLabel label $ TestCase $
    assertFailure $ "Error while parsing: " ++ (show err)

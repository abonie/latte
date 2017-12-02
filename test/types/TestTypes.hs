import Test.HUnit
import System.Directory
import System.FilePath
import Control.Monad (when)
import AbsLatte (Program, Type(..))
import ParLatte (pProgram)
import LexLatte (tokens)
import ErrM (Err(..))
import TypeCheck (runTypeCheck, typeCheck, TypeError(..))


main = do
    ls <- getCurrentDirectory >>= getDirectoryContents
    testFiles <- mapM readFile $ filter ((==".lat") . takeExtension) ls
    let testProgs = map (pProgram . tokens) testFiles
    when (any isBad testProgs) (error $ show $ head $ filter isBad testProgs)
    runTestTT $ TestList $ map (testTypeMismatch) (takeOk testProgs)


isBad :: Err a -> Bool
isBad (Ok _) = False
isBad (Bad _) = True


takeOk :: [Err a] -> [a]
takeOk xs = map (\(Ok x) -> x) (filter (not . isBad) xs)


testTypeMismatch :: Program -> Test
testTypeMismatch prog = TestCase (
    case runTypeCheck $ typeCheck prog of
        Right _ -> assertFailure "Expected TypeMismatch error, got none"
        Left err -> assertBool ("Expected TypeMismatch error, got " ++ (show err))
                               (case err of { TypeMismatch _ _ -> True; _ -> False }))
--testTypeMismatch prog terr = TestCase (
--    case runTypeCheck $ typeCheck prog of
--        Right _ -> assertFailure $ "Expected " ++ (show terr) ++ ", got none"
--        Left err -> assertEqual ("Expected " ++ (show terr) ++ ", got " ++ (show err)) terr err)

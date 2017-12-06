import Test.HUnit
import System.Directory
import System.FilePath
import Control.Monad (when)
import AbsLatte (Program, Type(..))
import ParLatte (pProgram)
import LexLatte (tokens)
import ErrM (Err(..))
import TypeCheck (runTypeCheck, typeCheck, TypeError(..))


tests = [
    ("test1.lat", "", TypeMismatch Int Bool),
    ("test2.lat", "", TypeMismatch Str Int),
    ("bad003.lat", "", MultipleDeclarations),
    ("bad007.lat", "", MultipleDeclarations),
    ("bad017.lat", "", TypeMismatch Int Bool),  -- TODO
    ("bad020.lat", "", TypeMismatch Str Bool),
    ("bad022.lat", "", TypeMismatch Str Int),
    ("bad023.lat", "", TypeMismatch Int Str),
    ("bad026.lat", "", TypeMismatch Int Str),
    ("bad027.lat", "", TypeMismatch Str Int)]


main = do
    args <- mapM foo tests
    when (any (\(x,_,_) -> isBad x) args)
         (error $ show $ head $ filter isBad $ map (\(x, _, _) -> x) args)
    runTestTT $ TestList $ map (\(Ok p, l, e) -> testTypeMismatch p l e) args
  where
    foo :: (FilePath, String, TypeError) -> IO (Err Program, String, TypeError)
    foo (filename, label, err) = do
        f <- readFile filename
        return (pProgram $ tokens f, label, err)


isBad :: Err a -> Bool
isBad (Ok _) = False
isBad (Bad _) = True


testTypeMismatch :: Program -> String -> TypeError -> Test
testTypeMismatch prog label terr = TestLabel label $ TestCase (
    case runTypeCheck $ typeCheck prog of
        Right _ -> assertFailure "Expected TypeError error, got none"
        Left err -> assertEqual ("Expected " ++ (show terr) ++ ", got " ++ (show err)) err terr)

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
    ("test1.lat", "test1.lat", TypeMismatch Int Bool),
    ("test2.lat", "test2.lat", TypeMismatch Str Int),
    ("test3.lat", "test3.lat", TypeMismatch Str Int),
    ("test4.lat", "test4.lat", TypeMismatch Int Bool),
    ("test5.lat", "test5.lat", TypeMismatch Int Void),
    ("bad003.lat", "bad003.lat", MultipleDeclarations),
    ("bad007.lat", "bad007.lat", MultipleDeclarations),
    ("bad010.lat", "bad010.lat", TypeMismatch Int Void),
    ("bad017.lat", "bad017.lat", TypeMismatch (Fun Int [Int]) (Fun Int [])),
    ("bad018.lat", "bad018.lat", TypeMismatch (Fun Int [Int, Int]) (Fun Int [Int])),
    ("bad019.lat", "bad019.lat", TypeMismatch (Fun Int [Int]) (Fun Int [Int, Int])),
    ("bad020.lat", "bad020.lat", TypeMismatch Str Bool),
    ("bad021.lat", "bad021.lat", NoReturn),
    ("bad022.lat", "bad022.lat", TypeMismatch Str Int),
    ("bad023.lat", "bad023.lat", TypeMismatch Int Str),
    ("bad026.lat", "bad026.lat", TypeMismatch Int Str),
    ("bad027.lat", "bad027.lat", TypeMismatch Str Int)]


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
        Right _ -> assertFailure $ "Expected " ++ (show terr) ++ " error, got none"
        Left err -> assertEqual ("Expected " ++ (show terr) ++ ", got " ++ (show err)) err terr)

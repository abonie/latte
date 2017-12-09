import Test.HUnit
import System.Directory
import System.FilePath
import Control.Monad (when)
import AbsLatte
import ParLatte (pProgram)
import LexLatte (tokens)
import ErrM (Err(..))
import TypeCheck (runTypeCheck, typeCheck, TypeError(..))


tests = [
    ("test1.lat", "test1.lat", TypeMismatch pInt pBool),
    ("test2.lat", "test2.lat", TypeMismatch pStr pInt),
    ("test3.lat", "test3.lat", TypeMismatch pStr pInt),
    ("test4.lat", "test4.lat", TypeMismatch pInt pBool),
    ("test5.lat", "test5.lat", TypeMismatch pInt pVoid),
    ("bad003.lat", "bad003.lat", MultipleDeclarations),
    ("bad007.lat", "bad007.lat", MultipleDeclarations),
    ("bad010.lat", "bad010.lat", TypeMismatch pInt pVoid),
    ("bad017.lat", "bad017.lat", TypeMismatch (pFun pInt [pInt]) (pFun pInt [])),
    ("bad018.lat", "bad018.lat", TypeMismatch (pFun pInt [pInt, pInt]) (pFun pInt [pInt])),
    ("bad019.lat", "bad019.lat", TypeMismatch (pFun pInt [pInt]) (pFun pInt [pInt, pInt])),
    ("bad020.lat", "bad020.lat", TypeMismatch pStr pBool),
    ("bad021.lat", "bad021.lat", NoReturn),
    ("bad022.lat", "bad022.lat", TypeMismatch pStr pInt),
    ("bad023.lat", "bad023.lat", TypeMismatch pInt pStr),
    ("bad026.lat", "bad026.lat", TypeMismatch pInt pStr),
    ("bad027.lat", "bad027.lat", TypeMismatch pStr pInt)]


main = do
    args <- mapM parseTests tests
    when (any (\(x,_,_) -> isBad x) args)
         (error $ show $ head $ filter isBad $ map (\(x, _, _) -> x) args)
    runTestTT $ TestList $ map (\(Ok p, l, e) -> testTypeMismatch p l e) args
  where
    parseTests :: (FilePath, String, TypeError) -> IO (Err PProgram, String, TypeError)
    parseTests (filename, label, err) = do
        f <- readFile filename
        return (pProgram $ tokens f, label, err)


isBad :: Err a -> Bool
isBad (Ok _) = False
isBad (Bad _) = True


testTypeMismatch :: PProgram -> String -> TypeError -> Test
testTypeMismatch prog label terr = TestLabel label $ TestCase (
    case runTypeCheck $ typeCheck prog of
        Right _ -> assertFailure $ "Expected " ++ (show terr) ++ " error, got none"
        Left err -> assertBool ("Expected " ++ (show terr) ++ ", got " ++ (show err)) (matchError err terr))


matchError :: TypeError -> TypeError -> Bool
matchError (TypeMismatch t1 t2) (TypeMismatch u1 u2) = let
    t1' = rmpos t1
    t2' = rmpos t2
    u1' = rmpos u1
    u2' = rmpos u2
    in (TypeMismatch t1' t2') == (TypeMismatch u1' u2')

matchError a b = a == b

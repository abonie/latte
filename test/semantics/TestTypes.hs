import Test.HUnit
import System.Directory
import System.FilePath
import Control.Monad (when)
import Data.Either
import AbsLatte
import ParLatte (pProgram)
import LexLatte (tokens)
import ErrM (Err(..))
import TypeError
import TypeCheck (typeCheck)


badTests :: [(String, String, PosInfo -> TypeError)]
badTests = [
    ("test1.lat", "test1.lat", typeMismatch pInt pBool),
    ("test2.lat", "test2.lat", typeMismatch pStr pInt),
    ("test3.lat", "test3.lat", typeMismatch pStr pInt),
    ("test4.lat", "test4.lat", typeMismatch pInt pBool),
    ("test5.lat", "test5.lat", typeMismatch pInt pVoid),
    ("test6.lat", "test6.lat", noReturn (Ident "")),
    ("badfun.lat", "badfun.lat", multipleDeclarations (Ident "")),
    ("bad003.lat", "bad003.lat", multipleDeclarations (Ident "")),
    ("bad007.lat", "bad007.lat", multipleDeclarations (Ident "")),
    ("bad008.lat", "bad008.lat", noReturn (Ident "")),
    ("bad010.lat", "bad010.lat", typeMismatch pInt pVoid),
    ("bad012.lat", "bad012.lat", noReturn (Ident "")),
    ("bad017.lat", "bad017.lat", typeMismatch (pFun pInt [pInt]) (pFun pInt [])),
    ("bad018.lat", "bad018.lat", typeMismatch (pFun pInt [pInt, pInt]) (pFun pInt [pInt])),
    ("bad019.lat", "bad019.lat", typeMismatch (pFun pInt [pInt]) (pFun pInt [pInt, pInt])),
    ("bad020.lat", "bad020.lat", typeMismatch pStr pBool),
    ("bad021.lat", "bad021.lat", noReturn (Ident "")),
    ("bad022.lat", "bad022.lat", typeMismatch pStr pInt),
    ("bad023.lat", "bad023.lat", typeMismatch pInt pStr),
    ("bad024.lat", "bad024.lat", noReturn (Ident "")),
    ("bad025.lat", "bad025.lat", noReturn (Ident "")),
    ("bad026.lat", "bad026.lat", typeMismatch pInt pStr),
    ("bad027.lat", "bad027.lat", typeMismatch pStr pInt)]


goodTests :: [String]
goodTests = [
    "goodblock5.lat",
    "goodblock4.lat",
    "goodblock3.lat",
    "goodblock2.lat",
    "goodblock.lat",
    "goodfun.lat"]

main = do
    args <- mapM parseTests badTests
    when (any (\(x,_,_) -> isBad x) args)
         (error $ show $ head $ filter isBad $ map (\(x, _, _) -> x) args)
    runTestTT $ TestList $ map (\(Ok p, l, e) -> testTypeError p l e) args
    args'' <- mapM readFile goodTests
    let args' = map (pProgram . tokens) args''
    when (any isBad args') (error $ show $ head $ filter isBad args')
    runTestTT $ TestList $ map (\(Ok p, l) -> makeTypeTest p l) $ zip args' goodTests
  where
    parseTests :: (FilePath, String, PosInfo -> TypeError) -> IO (Err PProgram, String, TypeError)
    parseTests (filename, label, err) = do
        f <- readFile filename
        return (pProgram $ tokens f, label, err nopos)


isBad :: Err a -> Bool
isBad (Ok _) = False
isBad (Bad _) = True


makeTypeTest :: PProgram -> String -> Test
makeTypeTest prog label = TestLabel label $ TestCase $
    case typeCheck prog of
        Left err -> assertFailure $ "Unexpected type error: \"" ++ (show err) ++ "\""
        Right _ -> assertBool "" True  -- TODO

testTypeError :: PProgram -> String -> TypeError -> Test
testTypeError prog label terr = TestLabel label $ TestCase (
    case typeCheck prog of
        Right _ -> assertFailure $ "Expected " ++ (show terr) ++ " error, got none"
        Left err -> assertBool ("Expected " ++ (show terr) ++ ", got " ++ (show err)) (matchError err terr))


matchError :: TypeError -> TypeError -> Bool
matchError (TypeMismatch _ t1 t2) (TypeMismatch _ u1 u2) = let
    t1' = rmpos t1
    t2' = rmpos t2
    u1' = rmpos u1
    u2' = rmpos u2
    in (TypeMismatch "" t1' t2') == (TypeMismatch "" u1' u2')

matchError a b = ("" <$ a) == ("" <$ b)

module Semantics.TypesSpec (spec) where
import System.FilePath (combine)
import Control.Monad (forM_)
import Data.Either (isRight, isLeft)
import Test.Hspec
import Parsing (parse)
import Parsing.AbsLatte
import Semantics.TypeCheck (typeCheck)
import Errors.LatteError


spec :: Spec
spec = do
    describe "typeCheck" $ do
        forM_ testCases $ \(fname, pred, behaves) ->
            it behaves $ do
                source <- readFile $ combine fixturesDir fname
                (parse source >>= typeCheck) `shouldSatisfy` pred


-- TODO better behaviour description
testCases :: [(FilePath, (Either (LatteError PType) a -> Bool), String)]
testCases = [
    ("test1.lat", isTypeMismatch pInt pBool, "throws"),
    ("test2.lat", isTypeMismatch pStr pInt, "throws"),
    ("test3.lat", isTypeMismatch pStr pInt, "throws"),
    ("test4.lat", isTypeMismatch pInt pBool, "throws"),
    ("test5.lat", isTypeMismatch pInt pVoid, "throws"),
    ("test6.lat", isNoReturn, "throws"),
    ("badfun.lat", isMultipleDeclarations, "throws"),
    --("bad2da.lat", isOtherError, "throws"), XXX TODO
    ("bad003.lat", isMultipleDeclarations, "throws"),
    ("bad007.lat", isMultipleDeclarations, "throws"),
    ("bad008.lat", isNoReturn, "throws"),
    ("bad010.lat", isTypeMismatch pInt pVoid, "throws"),
    ("bad012.lat", isNoReturn, "throws"),
    ("bad017.lat", isTypeMismatch (pFun pInt [pInt]) (pFun pInt []), "throws"),
    ("bad018.lat", isTypeMismatch (pFun pInt [pInt, pInt]) (pFun pInt [pInt]), "throws"),
    ("bad019.lat", isTypeMismatch (pFun pInt [pInt]) (pFun pInt [pInt, pInt]), "throws"),
    ("bad020.lat", isTypeMismatch pStr pBool, "throws"),
    ("bad021.lat", isNoReturn, "throws"),
    ("bad022.lat", isTypeMismatch pStr pInt, "throws"),
    ("bad023.lat", isTypeMismatch pInt pStr, "throws"),
    ("bad024.lat", isNoReturn, "throws"),
    ("bad025.lat", isNoReturn, "throws"),
    ("bad026.lat", isTypeMismatch pInt pStr, "throws"),
    ("bad027.lat", isTypeMismatch pStr pInt, "throws"),
    ("goodblock5.lat",  success, "accepts"),
    ("goodblock4.lat",  success, "accepts"),
    ("goodblock3.lat",  success, "accepts"),
    ("goodblock2.lat",  success, "accepts"),
    ("goodblock.lat",   success, "accepts"),
    --("goodcls.lat",     success, "accepts"), XXX TODO
    ("goodfun.lat",     success, "accepts"),
    ("printInt.lat",    success, "accepts builtin printInt"),
    ("printString.lat", success, "accepts builtin printString")
    ]


success :: Either a b -> Bool
success = isRight


isTypeMismatch :: Type PosInfo -> Type PosInfo -> Either (LatteError PType) a -> Bool
isTypeMismatch t1 t2 (Left (TErr (TypeMismatch _ u1 u2))) = let
    t1' = rmpos t1
    t2' = rmpos t2
    u1' = rmpos u1
    u2' = rmpos u2
    in t1' == u1' && t2' == u2'
isTypeMismatch _ _ _ = False


isNoReturn :: Either (LatteError PType) a -> Bool
isNoReturn (Left (TErr (NoReturn _))) = True
isNoReturn _ = False


isMultipleDeclarations :: Either (LatteError PType) a -> Bool
isMultipleDeclarations (Left (TErr (MultipleDeclarations _))) = True
isMultipleDeclarations _ = False


isOtherError :: Either (LatteError PType) a -> Bool
isOtherError (Left (TErr (OtherError _))) = True
isOtherError _ = False


fixturesDir :: FilePath
fixturesDir = combine "test" "fixtures"

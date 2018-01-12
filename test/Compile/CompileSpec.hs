module Compile.CompileSpec where
import System.FilePath (combine)
import Control.Monad (forM_)
import Data.Either (isRight, isLeft)
import Test.Hspec
import Parsing (parse)
import Semantics.TypeCheck (typeCheck)
import Compile.CodeGen (compile)


spec :: Spec
spec = do
    describe "compile" $ do
        forM_ testFiles $ \(fname, pred, behaves) ->
            it behaves $ do
                source <- readFile $ combine fixturesDir fname
                (parse source >>= typeCheck >>= compile) `shouldSatisfy` pred


testFiles :: [(FilePath, (Either a b) -> Bool, String)]
testFiles = [
        ("bad001.lat", failure, "Fails on unmatched comment"),
        ("bad002.lat", failure, "Fails on global identifier"),
        ("bad003.lat", failure, "Fails on repeated argument names"),
        ("bad004.lat", failure, "Fails on unmatched paren"),
        ("bad005.lat", failure, "Fails on illegal signature"),
        ("bad006.lat", failure, "Fails on undeclared variable"),
        ("bad007.lat", failure, "Fails on multiple declarations"),
        ("bad008.lat", failure, "Fails on missing return statement"),
        ("bad009.lat", failure, "Fails on type mismatch"),
        ("bad010.lat", failure, "Fails on illegal return type"),
        ("bad011.lat", failure, "Fails on illegal return type"),
        ("bad012.lat", failure, "Fails on missing return statement"),
        ("bad013.lat", failure, "Fails on type mismatch"),
        ("bad015.lat", failure, "Fails on type mismatch"),
        ("bad016.lat", failure, "Fails on type mismatch"),
        ("bad017.lat", failure, "Fails on too few arguments"),
        ("bad018.lat", failure, "Fails on too few arguments"),
        ("bad019.lat", failure, "Fails on too many arguments"),
        ("bad020.lat", failure, "Fails on type mismatch"),
        ("bad021.lat", failure, "Fails on missing return"),
        ("bad022.lat", failure, "Fails on type mismatch"),
        ("bad023.lat", failure, "Fails on type mismatch"),
        ("bad024.lat", failure, "Fails on missing return"),
        ("bad025.lat", failure, "Fails on missing return"),
        ("bad026.lat", failure, "Fails on type mismatch"),
        ("bad027.lat", failure, "Fails on type mismatch"),
        ("core001.lat", success, "Accepts"),
        ("core002.lat", success, "Accepts void expr. as statement"),
        ("core003.lat", success, "Accepts no return in unreachable branches"),
        ("core004.lat", success, "Accepts equality testing on booleans"),
        ("core005.lat", success, "Accepts variable initialized in both if-else branches"),
        ("core006.lat", success, "Accepts multiple declarations in one statement"),
        ("core007.lat", success, "Accepts declaration with initialization"),
        ("core008.lat", success, "Accepts multi. declarations w/ initialization"),
        ("core009.lat", success, "Accepts calling 0-param functions"),
        ("core010.lat", success, "Treats function params as initialized"),
        ("core011.lat", success, "Accepts printInt(-1)"),
        ("core012.lat", success, "Accepts arithmetic and comparison"),
        ("core013.lat", success, "Accepts boolean operators"),
        ("core014.lat", success, "Accepts"),
        ("core015.lat", success, "Accepts recursion"),
        ("core016.lat", success, "Accepts"),
        ("core017.lat", success, "Accepts boolean operators"),
        ("core018.lat", success, "Accepts readString"),
        ("core019.lat", success, "Accepts scope shenanigans"),
        ("core020.lat", success, "Accepts printInt and void function"),
        ("core021.lat", success, "Accepts no return in unreachable branch"),
        ("core022.lat", success, "Accepts printIn")
    ]


success :: Either a b -> Bool
success = isRight

failure :: Either a b -> Bool
failure = isLeft


fixturesDir :: FilePath
fixturesDir = combine "test" "fixtures"

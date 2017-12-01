import Test.HUnit
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader)
import Data.Map as Map
import AbsLatte (Program, Type(..))
import ParLatte (pProgram)
import LexLatte (tokens)
import ErrM (Err(..))
import TypeCheck (typeCheck, TypeError(..))


main = do
    f <- readFile "test1.lat"
    case pProgram $ tokens f of
        Ok prog -> runTestTT $ testTypeMismatch prog $ TypeMismatch Int Bool
        Bad err -> error err

testTypeMismatch :: Program -> TypeError -> Test
testTypeMismatch prog terr = TestCase (
    case runReader (runExceptT $ typeCheck prog) Map.empty of
        Right _ -> assertFailure $ "Expected " ++ (show terr) ++ ", got none"
        Left err -> assertEqual ("Expected " ++ (show terr) ++ ", got " ++ (show err)) terr err)

module Main where
import System.Environment (getArgs, getExecutablePath)
import System.FilePath (splitFileName, replaceExtension, combine)
import System.Process (callCommand)
import System.Exit (exitWith, exitSuccess, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import Parsing (parse)
import Parsing.ErrM (Err(..))
import Semantics.TypeCheck (typeCheck)
import Compile.CodeGen (compile)


main :: IO ()
main = do
    args <- getArgs
    when (length args == 0) (die "Usage: latc_llvm <file>" 1)
    let path = head args
    f <- readFile path
    case parse f >>= typeCheck >>= compile of
        Right _ -> do
            hPutStrLn stderr "OK"
            exitSuccess
        Left err -> do
            hPutStrLn stderr "Error"
            die (show err) 2


die :: String -> Int -> IO a
die str c = (hPutStrLn stderr str) >> (exitWith (ExitFailure c))

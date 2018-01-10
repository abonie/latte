module Main where
import System.Environment (getArgs, getExecutablePath)
import System.FilePath (splitFileName, replaceExtension, combine)
import System.Process (callCommand)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import Parsing (parse)
import Parsing.ErrM (Err(..))
import Semantics.TypeCheck (typeCheck)
import Compile.CodeGen (compile)
import LLVM.Printer (printTree)


main :: IO ()
main = do
    args <- getArgs
    when (length args == 0) (die "Usage: latc_llvm <file>" 1)
    let path = head args
    f <- readFile path
    case parse f >>= typeCheck >>= compile of
        Right llvm -> buildLLVM (printTree llvm) path
        Left err -> die (show err) 2


buildLLVM :: String -> String -> IO ()
buildLLVM src path = do
    let (dir, fname) = splitFileName path
    let outPath = combine dir $ replaceExtension fname "ll"
    let tmpBc = combine dir "tmp.bc"
    let outBc = replaceExtension outPath "bc"
    execPath <- getExecutablePath
    let execDir = fst $ splitFileName execPath
    let runtimeBc = combine execDir "lib/runtime.bc"
    writeFile outPath src
    callCommand $ "llvm-as " ++ outPath ++ " -o " ++ tmpBc
    callCommand $ "llvm-link " ++ tmpBc ++ " " ++ runtimeBc ++ " -o " ++ outBc
    callCommand $ "rm " ++ tmpBc


die :: String -> Int -> IO a
die str c = (hPutStrLn stderr str) >> (exitWith (ExitFailure c))

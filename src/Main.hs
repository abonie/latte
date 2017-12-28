module Main where
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import Parsing.ParLatte (pProgram)
import Parsing.LexLatte (tokens)
import Parsing.ErrM (Err(..))
import Semantics.TypeCheck (typeCheck)

main :: IO ()
main = do
    args <- getArgs
    when (length args == 0) (die "Usage: lattec <file>" 1)
    f <- readFile $ head args
    case pProgram $ tokens f of
        Ok ast -> case typeCheck ast of
            Right _ -> putStrLn "Great success!"
            Left err -> die ("Semantic error: " ++ (show err)) 3
        Bad err -> die ("Parsing error: " ++ err) 2

die :: String -> Int -> IO a
die str c = (hPutStrLn stderr str) >> (exitWith (ExitFailure c))

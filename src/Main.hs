module Main where
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import Parsing (parse)
import Parsing.ErrM (Err(..))
import Semantics.TypeCheck (typeCheck)

main :: IO ()
main = do
    args <- getArgs
    when (length args == 0) (die "Usage: lattec <file>" 1)
    f <- readFile $ head args
    case parse f >>= typeCheck of
        Right _ -> putStrLn "Great success!"
        Left err -> die (show err) 2


die :: String -> Int -> IO a
die str c = (hPutStrLn stderr str) >> (exitWith (ExitFailure c))

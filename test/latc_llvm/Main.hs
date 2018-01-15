module Main where
import System.Environment (getExecutablePath)
import System.Process (callCommand, readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.Directory (copyFile)
import System.FilePath (splitFileName, replaceExtension, combine)
import Control.Monad (forM_)


main :: IO ()
main = do
    forM_ testFiles $ \fname -> do
        putStrLn fname
        let fpath = combine fixtures fname
        should <- readFile $ replaceExtension fpath "output"
        execPath <- getExecutablePath
        let here = fst $ splitFileName execPath
        let fpath' = combine here fname
        let bc = replaceExtension fpath' "bc"
        copyFile fpath fpath'
        (code, _, err) <- readProcessWithExitCode latc_llvm [fpath'] ""
        if code == ExitSuccess then do
            (code, out, err) <- readProcessWithExitCode "lli" [bc] ""
            case code of
                ExitSuccess | out == should -> putStrLn "  OK"
                ExitSuccess -> do
                    putStrLn "  FAIL"
                    putStrLn "    got:"
                    putStrLn out
                    putStrLn "    expected:"
                    putStrLn should
                _ -> putStrLn $ "  FAIL lli: " ++ err
        else putStrLn $ "  FAIL: " ++ err
        callCommand $ "rm -f " ++ combine here "*.lat"
        callCommand $ "rm -f " ++ combine here "*.ll"
        callCommand $ "rm -f " ++ combine here "*.bc"


testFiles :: [FilePath]
testFiles = [
        "core001.lat",
        "core002.lat",
        --"core003.lat", -- return checker
        "core004.lat",
        "core005.lat",
        "core006.lat",
        "core007.lat",
        "core008.lat",
        "core009.lat",
        "core010.lat",
        "core011.lat",
        "core012.lat",
        "core013.lat",
        "core014.lat",
        "core015.lat",
        "core016.lat",
        "core017.lat",
        --"core018.lat", requires input
        "core019.lat",
        "core020.lat",
        --"core021.lat", -- return checker
        "core022.lat"
    ]        


fixtures :: FilePath
fixtures = combine ".." "fixtures"

latc_llvm :: FilePath
latc_llvm = combine ".." $ combine ".." "latc_llvm"
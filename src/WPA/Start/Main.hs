import System.Environment (getArgs)
import System.Process
import System.IO
import Data.List (isInfixOf)
import Control.Monad (unless)
import System.Exit (exitFailure)


desired = "CTRL-EVENT-CONNECTED"
    -- Уйти в фоновый режим после того как будет встречена эта подстрока


main :: IO ()
main = do
    args <- getArgs
    let (quiet, args') = if head args == "--quiet"
                         then (True, tail args)
                         else (False, args)
    (_, o, _, _) <- runInteractiveProcess "wpa_daemon" args' Nothing Nothing
    hSetBinaryMode o False
    hSetBuffering o NoBuffering
    parseLine quiet o


parseLine :: Bool -> Handle -> IO ()
parseLine quiet o = do
    eof <- hIsEOF o
    if eof
    then do
        hPutStrLn stderr "Daemon unexpectally terminated"
        exitFailure
    else do
        s <- hGetLine o
        unless quiet (putStrLn s)
        if desired `isInfixOf` s
        then putStrLn "Going to background"
        else parseLine quiet o

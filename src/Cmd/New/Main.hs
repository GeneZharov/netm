import System.Exit
import qualified Data.Set as S
import System.IO (hPutStrLn, stderr)

import Utils


main :: IO ()
main = do
    s <- loadStatus
    (_, n) <- parseArgs [] -- n — это new, новое множество имён
    new s n


new :: S.Set String -> S.Set String -> IO ()
new s n | S.null n = do
            hPutStrLn stderr "Required connection names"
            exitFailure
        | otherwise = do
            saveStatus n
            call (-1) "down" s
            call (-1) "up" n

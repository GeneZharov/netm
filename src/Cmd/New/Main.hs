import System.Exit
import Utils
import qualified Data.Set as S
import System.IO (hPutStrLn, stderr)


main :: IO ()
main = do
    s <- loadStatus
    (_, n) <- parseArgs -- n — это new, новое множество имён
    new s n


new :: S.Set String -> S.Set String -> IO ()
new s n | S.null n = do
            hPutStrLn stderr "Required connection names"
            exitFailure
        | otherwise = do
              saveStatus n
              call "down" s
              call "up" n

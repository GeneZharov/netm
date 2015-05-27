import Control.Monad (when)
import System.Exit
import qualified Data.Set as S
import System.IO (hPutStrLn, stderr)
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)

import Utils


main :: IO ()
main = do
    s <- loadStatus
    (_, n) <- parseArgs [] -- n — это new, новое множество имён
    (_, err) <- runStateT (new s n) False
    when err $ exitWith (ExitFailure 2)


new :: S.Set FilePath -> S.Set FilePath -> StateT Bool IO ()
new s n
    | S.null n = liftIO $ do
        hPutStrLn stderr "Required connection names"
        exitFailure
    | otherwise = do
        liftIO $ saveStatus n
        call (-1) "down" s
        call (-1) "up" n

import Control.Monad (unless)
import qualified Data.Set as S
import System.IO.Silently (silence)

import Utils


main :: IO ()
main = do
    s <- loadStatus
    (opts, n) <- parseArgs -- n — это new, новое множество имён
    (if "--quiet" `S.member` opts then silence else id)
        $ down s n ("--suspend" `S.member` opts)


down :: S.Set String -> S.Set String -> Bool -> IO ()
down s n suspend
    | S.null n = do
        putStrLn "Terminating all connections..."
        unless suspend (saveStatus S.empty)
        call "down" s
    | otherwise = do
        saveStatus $ S.filter ( \f -> not (S.member f n) ) s
        call "down" n

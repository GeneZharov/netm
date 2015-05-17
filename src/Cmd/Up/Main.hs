import Control.Monad (unless)
import qualified Data.Set as S
import System.IO.Silently (silence)

import Utils


main :: IO ()
main = do
    s <- loadStatus
    (opts, n) <- parseArgs -- n — это new, новое множество имён
    (if "--quiet" `S.member` opts then silence else id)
        $ up s n ("--resume" `S.member` opts)


up :: S.Set String -> S.Set String -> Bool -> IO ()
up s n resume
    | S.null n = do
        putStrLn "Restarting all connections..."
        unless resume (call "down" s)
        call "up" s
    | otherwise = do
        saveStatus (S.union s n)
        call "down" (S.intersection s n)
        call "up" n

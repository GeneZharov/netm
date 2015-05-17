import Utils
import qualified Data.Set as S


main :: IO ()
main = do
    s <- loadStatus
    n <- argsToFiles -- new, новое множество имён
    up s n


up :: S.Set String -> S.Set String -> IO ()
up s n
    | S.null n = do
        putStrLn "Restarting all connections..."
        call "down" s
        call "up" s
    | otherwise = do
        saveStatus (S.union s n)
        call "down" (S.intersection s n)
        call "up" n

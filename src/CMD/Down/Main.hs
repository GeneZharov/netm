import Utils
import qualified Data.Set as S


main :: IO ()
main = do
    s <- loadStatus
    n <- argsToFiles -- new, новое множество имён
    down s n


down :: S.Set String -> S.Set String -> IO ()
down s n
    | S.null n = do
        putStrLn "Terminating all connections..."
        saveStatus S.empty
        call "down" s
    | otherwise = do
        saveStatus $ S.filter ( \f -> not (S.member f n) ) s
        call "down" n

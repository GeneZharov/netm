import Utils
import qualified Data.Set as S


main :: IO ()
main = do
    s <- loadStatus
    n <- argsToFiles -- new, новое множество имён
    new s n


new :: S.Set String -> S.Set String -> IO ()
new s n = do
    saveStatus n
    call "down" s
    call "up" n

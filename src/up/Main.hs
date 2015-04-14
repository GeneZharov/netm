import Utils
import Control.Monad
import qualified Data.Set as S


main :: IO ()
main = do
    s <- loadStatus
    n <- argsToFiles -- new, новое множество имён
    up s n


up :: S.Set String -> Either [(String, [String])] (S.Set String) -> IO ()
up s (Left wired) = do
    mapM_ reportErr wired
    where reportErr (abbr, files)
            | null files = putStr "No conifgs found: " >> print abbr
            | otherwise  = do
                putStr "Non-obvious config name: "
                forM_ wired $ \(abbr, files) -> do
                    putStrLn abbr
                    forM_ files $ putStrLn . (++) (replicate 2 ' ')
up s (Right n)
    | S.null n = do
        call "down" s
        call "up" s
    | otherwise = do
        saveStatus (S.union s n)
        call "down" (S.intersection s n)
        call "up" n

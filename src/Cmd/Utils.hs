module Utils
   (
     parseArgs
   , loadStatus
   , saveStatus
   , call
   ) where


import System.Process
import System.Environment(getArgs)
import Control.Monad
import System.IO -- Работа с файлами
import qualified System.FilePath.Glob as G ( globDir, compile )
import qualified Data.Set as S
import System.FilePath
import Data.List.Split
import Data.List (intercalate, stripPrefix, isPrefixOf)
import System.Exit (exitFailure)
import Text.Printf
import System.Console.ANSI -- для цветного вывода
import Data.Maybe (fromJust)


etcDir = "/etc/netm/"           -- Каталог конфигов пользователя
stFile = "/var/lib/netm/active" -- Status, файл с текущими соединениями


-- Извлечение опций и имён конфигов из аргументов командной строки
parseArgs :: IO (S.Set String, S.Set String)
parseArgs = do

    args <- getArgs
    let (opts, files) = span (isPrefixOf "-") args
    files' <- argsToFiles files
    return (S.fromList opts, files')

    where

        -- Получение имён конфигов на основе аргументов командной строки
        argsToFiles :: [String] -> IO (S.Set String)
        argsToFiles files = do

            files <- mapM getFiles files
            let wired = flip filter files
                      $ \ (_, fs) -> let l = length fs in l > 1 || l == 0
            unless (null wired) (mapM_ reportErr wired >> exitFailure)
            return . S.fromList . map (head . snd) $ files

        -- Находит файлы подходящие под сокращение имени конфига
        getFiles :: String -> IO (String, [String])
        getFiles abbr = do
            files <- liftM (head . fst)
                       $ G.globDir [ G.compile (toPattern abbr) ] etcDir
            let names = map (fromJust . stripPrefix etcDir) files
            return (abbr, names)

        -- Формирует из сокращения имени конфига sh-шаблон для поиска файлов
        toPattern :: String -> String
        toPattern = intercalate "/" . map (++"*") . splitOn "/"
          -- "do/wl" -> "do*/wl*" — сматчится на "dolphin/wlan"

        reportErr :: (String, [String]) -> IO ()
        reportErr (abbr, files)
            | null files = putStr "No conifgs found: " >> print abbr
            | otherwise  = do
                putStrLn ("Non-obvious config name: " ++ abbr)
                forM_ files $ putStrLn . (++) (replicate 2 ' ')


loadStatus :: IO (S.Set String)
loadStatus = withFile stFile ReadMode $
    \h -> hGetContents h >>= readIO :: IO (S.Set String)


saveStatus :: S.Set String -> IO ()
saveStatus status = withFile stFile WriteMode $
    \h -> hPrint h status


-- Запускает множество пользовательских конфигов с заданной командой
call :: String -> S.Set String -> IO ()
call cmd files = forM_ (S.toList files) $ \ f -> do
    printHeader $ printf "Executing: %s %s" f cmd
    makeProcess f >>= wait

    where printHeader text = do
              setSGR [ SetColor Foreground Dull Green ]
              putStrLn ('\n':text)
              putStr $ replicate (length text) '━'
              setSGR [ Reset ] >> putStrLn ""

          makeProcess f = let f' = etcDir ++ f
                          in createProcess (proc f' [cmd])
                              {
                                  cwd = Just (dropFileName f')
                              ,   delegate_ctlc = True
                              }

          wait (_, _, _, h) = waitForProcess h

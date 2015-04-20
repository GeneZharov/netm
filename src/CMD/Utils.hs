module Utils
   (
     argsToFiles
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
import Data.List (intercalate)
import System.Exit (exitFailure)


etcDir = "/etc/netm/"           -- Каталог конфигов пользователя
stFile = "/var/lib/netm/active" -- Status, файл с текущими соединениями


-- Получение имён конфигов на основе аргументов командной строки
type Abbr = String
argsToFiles :: IO (S.Set String)
argsToFiles = do
    files <- getArgs >>= mapM getFiles
    let wired = flip filter files
              $ \ (_, fs) -> let l = length fs in l > 1 || l == 0
    when (not $ null wired) (mapM_ reportErr wired >> exitFailure)
    return . S.fromList . map (head . snd) $ files
    where
        -- Находит файлы подходящие под сокращение имени соединения
        getFiles :: String -> IO (String, [String])
        getFiles abbr = liftM ((,) abbr . head . fst)
                      $ G.globDir [ G.compile (toPattern abbr) ] etcDir
        -- Формирует из сокращения имени соединения sh-шаблон для поиска файлов
        toPattern :: String -> String
        toPattern = intercalate "/" . map (++"*") . splitOn "/"
        --toPattern = intercalate "/" . map (concatMap (:"*")) . splitOn "/"
          -- "do/wl" -> "do*/wl*" — сматчится на "dolphin/wlan"

        reportErr :: (Abbr, [String]) -> IO ()
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
call cmd files = forM_ (S.toList files) (makeProcess >=> wait)
    where makeProcess f = createProcess (proc f [cmd]) {
            cwd = Just (dropFileName f),
            delegate_ctlc = True -- делегировать управление с помощью ctrl-c
          }
          wait (_, _, _, h) = waitForProcess h

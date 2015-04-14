module Utils
   (
     argsToFiles
   , loadStatus
   , saveStatus
   , call
   ) where


import System.Process
import System.Environment( getArgs )
import Control.Monad
import System.IO -- Работа с файлами
import qualified System.FilePath.Glob as G ( globDir, compile )
import qualified Data.Set as S
import System.FilePath
import Data.List.Split
import Data.List (intercalate)


etcDir = "/etc/netm/"           -- Каталог конфигов пользователя
stFile = "/var/lib/netm/active" -- Status, файл с текущими соединениями


-- Получение имён конфигов из аргументов командной строки
type Abbr = String
argsToFiles :: IO (Either [(Abbr, [String])] (S.Set String))
argsToFiles = do
    files <- getArgs >>= mapM getFiles
    let wired = flip filter files $ \ (_, fs) -> let l = length fs
                                                 in l > 1 || l == 0
    return $ if not (null wired)
             then Left wired
             else Right . S.fromList . map (head . snd) $ files
    where
        -- Находит файлы подходящие под сокращение имени соединения
        getFiles :: String -> IO (String, [String])
        getFiles abbr = liftM ((,) abbr . head . fst)
                      $ G.globDir [ G.compile (toPattern abbr) ] etcDir
        -- Формирует из сокращения имени соединения sh-шаблон для поиска файлов
        toPattern :: String -> String
        toPattern = intercalate "/" . map (concatMap (:"*")) . splitOn "/"
          -- "dp/wl" -> "d*p*/w*l*" — для поиска "dolphin/wlan"


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

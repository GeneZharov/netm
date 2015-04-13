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


etcDir = "/etc/netm/"           -- Каталог конфигов пользователя
stFile = "/var/lib/netm/active" -- Status, файл с текущими соединениями


-- Получение имён конфигов из аргументов командной строки
argsToFiles :: IO (S.Set String)
argsToFiles = do
    args <- getArgs
    files <- forM args (\c -> do
            matched <- G.globDir [ G.compile (c ++ "*") ] etcDir
            return $ head $ head $ fst matched
            -- Извлекаю первый из подходящих файлов для 1-го принятого шаблона
        )
    return (S.fromList files)


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

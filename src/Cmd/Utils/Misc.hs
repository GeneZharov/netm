module Utils.Misc where


import System.Process
import System.IO -- работа с файлами
import Control.Monad
import System.FilePath
import Text.Printf
import System.Console.ANSI -- для цветного вывода
import System.Exit
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State


etcDir = "/etc/netm/"           -- каталог конфигов пользователя
stFile = "/var/lib/netm/active" -- status, файл с текущими соединениями

type Abbr   = String -- сокращение имени конфига, например n/w
type Config = String -- полное имя конфига, например nest/wlan


loadStatus :: IO [Config]
loadStatus = withFile stFile ReadMode $
    \h -> hGetContents h >>= readIO :: IO [Config]


saveStatus :: [Config] -> IO ()
saveStatus status = withFile stFile WriteMode $
    \h -> hPrint h status


-- Запускает множество пользовательских конфигов с заданной командой
--runConfigs :: Int -> String -> [Config] -> State Bool ()
runConfigs timeout action files =

   liftIO callIO >>= modify . (||)

   where

     callIO :: IO Bool
     callIO = liftM or . forM files $ \ f -> do
        let cmd = unwords [ f, action ]
        printHeader $ printf ("Executing: " ++ cmd)
        execute timeout f >>= handleError cmd

     printHeader :: String -> IO ()
     printHeader text = do
        setSGR [ SetColor Foreground Dull Green ]
        putStrLn ('\n':text)
        putStr $ replicate (length text) '━'
        setSGR [ Reset ] >> putStrLn ""

     execute :: Int -> Config -> IO ExitCode
     execute timeout f =
        let f' = etcDir ++ f
            cmd = printf "timeout --foreground %d %s %s" timeout f' action
        in do
           (_,_,_,p) <- createProcess (shell cmd)
             { cwd = Just (dropFileName f')
             , delegate_ctlc = True
             }
           waitForProcess p

     handleError :: String -> ExitCode -> IO Bool
     handleError cmd ExitSuccess = return False
     handleError cmd (ExitFailure e)
       | e == 124 || e == 128+9 = report $ printf "User script timeout: %s" cmd
       | otherwise = report $ printf "User script error (%d): %s" e cmd
       where report msg = hPutStrLn stderr msg >> return True

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
import System.Console.GetOpt
import System.IO.Silently (silence)

import Utils.Common
import Utils.Args


-- Сохранение данных в файл и загрузка
load file      = withFile file ReadMode  (hGetContents >=> readIO)
save file info = withFile file WriteMode (`hPrint` info)


-- Запускает пользовательскую функцию, передавая в неё опции запуска, 
-- запрашиваемые соединения, текущие соединения
inEnv :: String
      -> [OptDescr Option]
      -> ( [Option]
         -> [Name]     -- request
         -> [Name]     -- status
         -> [Relation] -- hierarchy
         -> StateT Bool IO ()
         )
      -> IO ()
inEnv usage opts cmd = do
   st           <- load statusFile    :: IO [Name]
   hc           <- load hierarchyFile :: IO [Relation]
   (opts', req) <- parseArgs usage opts
   (_, err)     <- verbosity opts'
                 $ flip runStateT False
                 $ cmd opts' req st hc
   when err $ exitWith (ExitFailure 2)

   where verbosity opts = if Quiet `elem` opts
                          then silence
                          else id


-- Запускает множество пользовательских конфигов с заданной командой
--runConfigs :: Int -> String -> [Name] -> State Bool ()
runConfigs timeout action files =

   liftIO runConfigsIO >>= modify . (||)

   where

     runConfigsIO :: IO Bool
     runConfigsIO = liftM or . forM files $ \ f -> do
        let cmd = unwords [ f, action ]
        printHeader $ printf ("Executing: " ++ cmd)
        execute timeout f >>= handleError cmd

     printHeader :: String -> IO ()
     printHeader text = do
        setSGR [ SetColor Foreground Dull Green ]
        putStrLn ('\n':text)
        putStr $ replicate (length text) '━'
        setSGR [ Reset ] >> putStrLn ""

     execute :: Int -> Name -> IO ExitCode
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

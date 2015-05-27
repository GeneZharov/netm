module Utils where


import System.Process
import System.Environment (getArgs)
import Control.Monad
import System.IO -- работа с файлами
import qualified System.FilePath.Glob as G
import qualified Data.Set as S
import System.FilePath
import Data.List.Split
import Data.List (intercalate, stripPrefix, isPrefixOf)
import Text.Printf
import System.Console.ANSI -- для цветного вывода
import Data.Maybe (fromJust)
import System.Directory (getPermissions, executable)
import System.Console.GetOpt
import System.Exit
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)


etcDir = "/etc/netm/"           -- каталог конфигов пользователя
stFile = "/var/lib/netm/active" -- status, файл с текущими соединениями


-- Извлечение опций и имён конфигов из аргументов командной строки
parseArgs :: [OptDescr a] -> IO ([a], S.Set String)
parseArgs options = do

    -- Разбор опций командной строки
    argv <- getArgs
    (opts,files) <- case getOpt RequireOrder options argv of
                      (o,n,[]  ) -> return (o,n)
                      (_,_,errs) -> ioError $ userError (concat errs)

    -- Формирование имён конфигов
    files' <- argsToFiles files

    return (opts, files')

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
            files <- globDir abbr
            scripts <- filterM isScript files
            let names = map (fromJust . stripPrefix etcDir) scripts
            return (abbr, names)
            where

                -- Находит файлы, удовлетворяющие аббревиатуре конфига
                globDir :: String -> IO [FilePath]
                globDir abbr =
                    liftM (head . fst) -- совпавшие имена для первого шаблона
                    $ G.globDirWith
                        G.MatchOptions {
                              G.ignoreCase = True
                            , G.matchDotsImplicitly = False
                            , G.ignoreDotSlash = False
                            }
                        [ G.compile (toPattern abbr) ]
                        etcDir

                -- Является ли файл не каталогом и исполняемым
                isScript :: FilePath -> IO Bool
                isScript f =  liftM executable (getPermissions f)

                -- Сокращение имени конфига -> sh-шаблон для поиска файлов
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
--call :: Int -> String -> S.Set FilePath -> State Bool ()
call timeout action files =

   liftIO callIO >>= modify . (||)

   where

     callIO :: IO Bool
     callIO = liftM or . forM (S.toList files) $ \ f -> do
        let cmd = unwords [ f, action ]
        printHeader $ printf ("Executing: " ++ cmd)
        runScript timeout f >>= handleError cmd

     printHeader :: String -> IO ()
     printHeader text = do
        setSGR [ SetColor Foreground Dull Green ]
        putStrLn ('\n':text)
        putStr $ replicate (length text) '━'
        setSGR [ Reset ] >> putStrLn ""

     runScript :: Int -> FilePath -> IO ExitCode
     runScript timeout f =
        let f' = etcDir ++ f
            cmd = printf "timeout %d %s %s" timeout f' action
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

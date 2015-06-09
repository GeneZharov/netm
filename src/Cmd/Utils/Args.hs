module Utils.Args where

import System.Environment (getArgs)
import qualified System.FilePath.Glob as G
import Data.List.Split
import System.Exit
import System.Console.GetOpt
import System.Directory (getPermissions, executable)
import Data.Maybe (fromJust)
import Control.Monad
import Data.List (intercalate, stripPrefix, isPrefixOf)
import Control.Monad.Trans.State

import Utils.Common



-- Находит файлы подходящие под сокращение имени конфига
expandAbbr :: Abbr -> IO (Abbr, [Name])
expandAbbr abbr = do

    files   <- globDir abbr
    scripts <- isScript `filterM` files
    let names = (fromJust . stripPrefix etcDir) `map` scripts
    return (abbr, names)

    where

        -- Находит файлы, удовлетворяющие аббревиатуре конфига
        globDir :: Abbr -> IO [FilePath]
        globDir abbr =
            liftM (concat . fst) -- совпавшие имена для первого шаблона
            $ G.globDirWith
                G.MatchOptions {
                      G.ignoreCase = True
                    , G.matchDotsImplicitly = False
                    , G.ignoreDotSlash = False
                    }
                ( map G.compile (toPatterns abbr) )
                etcDir

        -- Является ли файл не каталогом и исполняемым
        isScript :: FilePath -> IO Bool
        isScript f =  liftM executable (getPermissions f)

        -- Сокращение имени конфига -> sh-шаблоны для поиска файлов
        toPatterns :: Abbr -> [String]
        toPatterns a = [ pattern
                       , pattern ++ "/**/*"
                       ]
            where pattern = intercalate "/" . map (++"*") . splitOn "/" $ a
                  -- "do/wl" -> "do*/wl*" — сматчится на "dolphin/wlan"



-- Извлечение опций и имён конфигов из аргументов командной строки
parseArgs :: String -> [OptDescr Option] -> IO ([Option], [Name])
parseArgs usage options = do

    -- Разбор опций командной строки
    argv <- getArgs
    (opts,files) <- case getOpt RequireOrder options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError $ userError (concat errs ++ usageInfo usage options)

    when (Help `elem` opts) $ do
        putStrLn (usageInfo usage options)
        exitSuccess

    -- Формирование имён конфигов
    files' <- if NoCompletion `elem` opts
              then return files
              else parseAbbrs files

    return (opts, files')

    where

        -- Получение имён конфигов на основе аргументов командной строки
        parseAbbrs :: [Abbr] -> IO [Name]
        parseAbbrs files = do
            files <- expandAbbr `mapM` files
            let wired = (`filter` files)
                      $ \ (_, fs) -> let l = length fs in l > 1 || l == 0
            unless (null wired) (mapM_ reportErr wired >> exitFailure)
            return . map (head . snd) $ files

        reportErr :: (Abbr, [Name]) -> IO ()
        reportErr (abbr, files)
            | null files = putStr "No conifgs found: " >> print abbr
            | otherwise  = do
                putStrLn ("Non-obvious config abbreviation: " ++ abbr)
                forM_ files $ putStrLn . (++) (replicate 2 ' ')

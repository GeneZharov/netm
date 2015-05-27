import Control.Monad (when, unless)
import qualified Data.Set as S
import System.IO.Silently (silence)
import System.Console.GetOpt
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import System.Exit

import Utils


data Option = Resume | Quiet | Timeout Int deriving (Eq, Show)
options :: [OptDescr Option]
options =
  [ Option "r" ["resume"] (NoArg Resume)
     "Восстановить состояние"
  , Option "q" ["quiet"]   (NoArg Quiet)
     "Подавить вывод stdout"
  , Option "t" ["timeout"] (ReqArg (Timeout . read) "INT")
     "Время ожидания пользовательского скрипта"
  ]


main :: IO ()
main = do

   s <- loadStatus
   (o, n) <- parseArgs options -- n — это new, новое множество имён
   (_, err) <- verbosity o
             $ flip runStateT False
             $ up (getTimeout o) (Resume `elem` o) s n
   when err $ exitWith (ExitFailure 2) -- один из конфигов вернул ошибку

   where verbosity o = if Quiet `elem` o then silence else id

         -- Формирует время ожидания на основе списка опций
         getTimeout :: [Option] -> Int
         getTimeout (Timeout t:os) = t
         getTimeout (_:os)         = getTimeout os
         getTimeout []             = 120 -- по умолчанию 2 минуты


-- В состоянии флаг, показывающий была ли ошибка в запуске какого-либо конфига
up :: Int -> Bool -> S.Set FilePath -> S.Set FilePath -> StateT Bool IO ()
up timeout resume s n
  | S.null n = if S.null s
               then liftIO $ putStrLn "Nothing to restart"
               else do
                    liftIO $ putStrLn "Restarting all connections..."
                    unless resume (call timeout "down" s)
                    call timeout "up" s
  | otherwise = do
      liftIO $ saveStatus (S.union s n)
      call timeout "down" (S.intersection s n)
      call timeout "up" n

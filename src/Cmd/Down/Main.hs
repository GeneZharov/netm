import Control.Monad (when, unless)
import System.Exit
import qualified Data.Set as S
import System.IO.Silently (silence)
import System.Console.GetOpt
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)

import Utils


data Option = Suspend | Quiet | Timeout Int deriving (Eq, Show)
options :: [OptDescr Option]
options =
  [ Option "s" ["suspend"] (NoArg Suspend)
     "Сохранить состояние"
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
             $ down (getTimeout o) (Suspend `elem` o) s n
   when err $ exitWith (ExitFailure 2)

   where verbosity o = if Quiet `elem` o then silence else id

         -- Формирует время ожидания на основе списка опций
         getTimeout :: [Option] -> Int
         getTimeout (Timeout t:os) = t
         getTimeout (_:os)         = getTimeout os
         getTimeout []             = 120 -- по умолчанию 2 минуты


down :: Int -> Bool -> S.Set FilePath -> S.Set FilePath -> StateT Bool IO ()
down timeout suspend s n
  | S.null n = do
      liftIO $ putStrLn "Terminating all connections..."
      liftIO $ unless suspend (saveStatus S.empty)
      call timeout "down" s
  | otherwise = do
      liftIO $ saveStatus $ S.filter ( \f -> not (S.member f n) ) s
      call timeout "down" n

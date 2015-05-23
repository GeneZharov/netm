import Control.Monad (unless)
import qualified Data.Set as S
import System.IO.Silently (silence)
import System.Console.GetOpt

import Utils


data Option = Resume | Quiet | Timeout Int deriving (Eq, Show)
options :: [OptDescr Option]
options =
  [ Option ['r'] ["resume"] (NoArg Resume)
     "Восстановить состояние"
  , Option ['q'] ["quiet"]   (NoArg Quiet)
     "Подавить вывод stdout"
  , Option ['t'] ["timeout"] (ReqArg (Timeout . read) "INT")
     "Время ожидания пользовательского скрипта"
  ]


main :: IO ()
main = do

   s <- loadStatus
   (o, n) <- parseArgs options -- n — это new, новое множество имён
   verbosity o $ up (getTimeout o) (Resume `elem` o) s n

   where verbosity o = if Quiet `elem` o then silence else id

         -- Формирует время ожидания на основе списка опций
         getTimeout :: [Option] -> Int
         getTimeout (Timeout t:os) = t
         getTimeout (_:os)         = getTimeout os
         getTimeout []             = 120 -- по умолчанию 2 минуты


up :: Int -> Bool -> S.Set String -> S.Set String -> IO ()
up timeout resume s n
  | S.null n = do
      putStrLn "Restarting all connections..."
      unless resume (call timeout "down" s)
      call timeout "up" s
  | otherwise = do
      saveStatus (S.union s n)
      call timeout "down" (S.intersection s n)
      call timeout "up" n

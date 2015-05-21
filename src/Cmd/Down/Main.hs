import Control.Monad (unless)
import qualified Data.Set as S
import System.IO.Silently (silence)
import System.Console.GetOpt

import Utils


data Option = Suspend | Quiet | Timeout Int deriving (Eq, Show)
options :: [OptDescr Option]
options =
  [ Option ['s'] ["suspend"] (NoArg Suspend)
     "Сохранить состояние"
  , Option ['q'] ["quiet"]   (NoArg Quiet)
     "Подавить вывод stdout"
  , Option ['t'] ["timeout"] (ReqArg (Timeout . read) "INT")
     "Время ожидания пользовательского скрипта"
  ]


main :: IO ()
main = do
    s <- loadStatus
    (o, n) <- parseArgs options -- n — это new, новое множество имён
    verbosity o $ down (getTimeout o) (Suspend `elem` o) s n
    where verbosity o = if Quiet `elem` o then silence else id
          -- Формирует время ожидания на основе списка опций
          getTimeout :: [Option] -> Int
          getTimeout (Timeout t:os) = t
          getTimeout (_:os)         = getTimeout os
          getTimeout []             = -1


down :: Int -> Bool -> S.Set String -> S.Set String -> IO ()
down timeout suspend s n
    | S.null n = do
        putStrLn "Terminating all connections..."
        unless suspend (saveStatus S.empty)
        call timeout "down" s
    | otherwise = do
        saveStatus $ S.filter ( \f -> not (S.member f n) ) s
        call timeout "down" n

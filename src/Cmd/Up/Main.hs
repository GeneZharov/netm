import Control.Monad (unless)
import qualified Data.Set as S
import System.Console.GetOpt
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)

import Utils


usage = "Usage: netu [OPTION...] [files...]"

opts :: [OptDescr Option]
opts =
  [ Option "r" ["resume"] (NoArg Resume)
     "Восстановить состояние"
  , Option "q" ["quiet"]   (NoArg Quiet)
     "Подавить вывод stdout"
  , Option "t" ["timeout"] (ReqArg (Timeout . read) "INT")
     "Время ожидания пользовательского скрипта"
  ]


main :: IO ()
main = inEnv usage opts $ \ opts req st
                         -> up (getTimeout opts) (Resume `elem` opts) st req


-- В состоянии флаг, показывающий была ли ошибка в запуске какого-либо конфига
up :: Int -> Bool -> S.Set FilePath -> S.Set FilePath -> StateT Bool IO ()
up timeout resume st req
  | S.null req = if S.null st
                 then liftIO $ putStrLn "Nothing to restart"
                 else do
                      liftIO $ putStrLn "Restarting all connections..."
                      unless resume (runConfigs timeout "down" st)
                      runConfigs timeout "up" st
  | otherwise = do
      liftIO $ saveStatus (S.union st req)
      runConfigs timeout "down" (S.intersection st req)
      runConfigs timeout "up" req

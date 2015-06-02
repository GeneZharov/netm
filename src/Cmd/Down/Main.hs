import Control.Monad (unless)
import System.Console.GetOpt
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Data.List (intersect)

import Utils


usage = "Usage: netd [OPTION...] [config...]"

opts :: [OptDescr Option]
opts =
  [ Option "s" ["suspend"] (NoArg Suspend)
     "Сохранить состояние"
  , Option "q" ["quiet"]   (NoArg Quiet)
     "Подавить вывод stdout"
  , Option "t" ["timeout"] (ReqArg (Timeout . read) "INT")
     "Время ожидания пользовательского скрипта"
  , Option "C" ["--no-completion"] (NoArg NoCompletion)
     "Не выполнять дополнение имён конфигов"
  ]


main :: IO ()
main = inEnv usage opts $ \ opts req st
                         -> down (getTimeout opts) (Suspend `elem` opts) req st


down :: Int -> Bool -> [FilePath] -> [FilePath] -> StateT Bool IO ()
down timeout suspend req st
  | null req = if null st
               then liftIO $ putStrLn "Nothing to shut down"
               else do
                    liftIO $ putStrLn "Terminating all connections..."
                    liftIO $ unless suspend (saveStatus [])
                    runConfigs timeout "down" (reverse st)
  | otherwise = do
      liftIO $ saveStatus $ filter ( \f -> not (elem f req) ) st
      runConfigs timeout "down" (reverse req)

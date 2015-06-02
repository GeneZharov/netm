import Control.Monad (unless)
import System.Console.GetOpt
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Data.List (intersect, (\\))

import Utils


usage = "Usage: netu [OPTION...] [config...]"

opts :: [OptDescr Option]
opts = commonOptions ++ [
    Option "r" ["resume"] (NoArg Resume) "Восстановить состояние"
  ]


main :: IO ()
main = inEnv usage opts $ \ opts req st
                         -> up (getTimeout opts) (Resume `elem` opts) req st


-- В состоянии флаг, показывающий была ли ошибка в запуске какого-либо конфига
up :: Int -> Bool -> [Config] -> [Config] -> StateT Bool IO ()
up timeout resume req st
  | null req = if null st
               then liftIO $ putStrLn "Nothing to restart"
               else do
                    liftIO $ putStrLn "Restarting all connections..."
                    unless resume $ runConfigs timeout "down" (reverse st)
                    runConfigs timeout "up" st
  | otherwise = do
      liftIO $ saveStatus ((st \\ req) ++ req)
      runConfigs timeout "down" (reverse (st `intersect` req))
      runConfigs timeout "up" req

import System.Console.GetOpt
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import System.IO (hPutStrLn, stderr)
import System.Exit
import Data.List (intersect)

import Utils


usage = "Usage: netn [OPTION...] files..."

opts :: [OptDescr Option]
opts =
  [ Option "q" ["quiet"]   (NoArg Quiet)
     "Подавить вывод stdout"
  , Option "t" ["timeout"] (ReqArg (Timeout . read) "INT")
     "Время ожидания пользовательского скрипта"
  , Option "C" ["--no-completion"] (NoArg NoCompletion)
     "Не выполнять дополнение имён конфигов"
  ]


main :: IO ()
main = inEnv usage opts $ \ opts req st
                         -> new (getTimeout opts) req st


new :: Int -> [Config] -> [Config] -> StateT Bool IO ()
new timeout req st
   | null req = liftIO $ do
       hPutStrLn stderr "Required connection names"
       exitFailure
   | otherwise = do
       liftIO $ saveStatus req
       runConfigs timeout "down" (reverse st)
       runConfigs timeout "up" req

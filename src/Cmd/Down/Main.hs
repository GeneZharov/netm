import Control.Monad (unless)
import qualified Data.Set as S
import System.Console.GetOpt
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)

import Utils


usage = "Usage: netd [OPTION...] [files...]"

opts :: [OptDescr Option]
opts =
  [ Option "s" ["suspend"] (NoArg Suspend)
     "Сохранить состояние"
  , Option "q" ["quiet"]   (NoArg Quiet)
     "Подавить вывод stdout"
  , Option "t" ["timeout"] (ReqArg (Timeout . read) "INT")
     "Время ожидания пользовательского скрипта"
  ]


main :: IO ()
main = inEnv usage opts $ \ opts req st
                         -> down (getTimeout opts) (Suspend `elem` opts) st req


down :: Int -> Bool -> S.Set FilePath -> S.Set FilePath -> StateT Bool IO ()
down timeout suspend st req
  | S.null req = if S.null st
                 then liftIO $ putStrLn "Nothing to shut down"
                 else do
                      liftIO $ putStrLn "Terminating all connections..."
                      liftIO $ unless suspend (saveStatus S.empty)
                      runConfigs timeout "down" st
  | otherwise = do
      liftIO $ saveStatus $ S.filter ( \f -> not (S.member f req) ) st
      runConfigs timeout "down" req

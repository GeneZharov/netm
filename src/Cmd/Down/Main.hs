import Control.Monad (unless)
import System.Console.GetOpt
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\))

import Utils.Common
import Utils.Options (commonOptions, getTimeout)
import Utils.Hierarchy (getDescendants)
import Utils.Misc (inEnv, runConfigs, save)


usage = "Usage: netd [option...] [config...]\n\
        \   or: netd --suspend [option...]\n\
        \   or: netd --help"


opts :: [OptDescr Option]
opts = commonOptions ++
  [ Option "s" ["suspend"] (NoArg Suspend) "Сохранить состояние" ]


main :: IO ()
main = inEnv usage opts $ \ opts req st hc
                         -> down ( getTimeout opts )
                                 ( Suspend `elem` opts )
                                 req  st  hc


down :: Int -> Bool
     -> [Name] -> [Name] -> [Relation]
     -> StateT Bool IO ()
     -- В состоянии — флаг, показывающий была ли ошибка в запуске любого конфига


down _ _ [] [] _ =
   liftIO $ putStrLn "Nothing to shut down"


-- Завершение всех соединений
down timeout suspend [] st hc = do
   liftIO $ unless suspend $ do
      save statusFile    ([] :: [Name])
      save hierarchyFile ([] :: [Relation])
   liftIO $ putStrLn "Terminating all connections..."
   runConfigs timeout "down" (reverse st)


-- Завершение заданных соединений
down timeout suspend req st hc = do
   let forDown = req >>= \ p -> p : getDescendants hc p
   liftIO $ do
      save statusFile (st \\ forDown)
      save hierarchyFile $
         (`filter` hc) $ \ (child, _) -> child `notElem` forDown
   runConfigs timeout "down" (reverse forDown)

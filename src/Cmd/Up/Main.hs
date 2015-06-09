import Control.Monad (unless, when)
import System.Console.GetOpt
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\), union, intersect)
import Data.Maybe (isJust, fromJust)
import System.IO
import System.Exit

import Utils.Common
import Utils.Options (commonOptions, getTimeout, getOwner)
import Utils.Hierarchy (getDescendants)
import Utils.Misc (inEnv, runConfigs, save)


usage = "Usage: netu [option...] [config...]\n\
        \   or: netu --owner=config [option...] config...\n\
        \   or: netu --resume [option...]\n\
        \   or: netu --help"


opts :: [OptDescr Option]
opts = commonOptions ++
  [ Option "r" ["resume"] (NoArg Resume) "Восстановить состояние"
  , Option "o" ["owner" ] (ReqArg Owner "STRING") "Имя конфига-владельца"
  ]


main :: IO ()
main = inEnv usage opts $ \ opts req st hc
                         -> up ( getTimeout    opts )
                               ( Resume `elem` opts )
                               ( getOwner      opts )
                               req  st  hc


up :: Int -> Bool
   -> Maybe Parent -> [Name] -> [Name] -> [Relation]
   -> StateT Bool IO ()
   -- В состоянии — флаг, показывающий была ли ошибка в запуске любого конфига


up _ _ _ [] [] _ =
    liftIO $ putStrLn "Nothing to restart"


-- Перезапуск всех соединений
up timeout resume Nothing [] st hc = do
    let noParents = st \\ fst `map` hc -- не имеющие своих родителей
    liftIO $ do
        putStrLn "Restarting all connections..."
        save statusFile noParents
        save hierarchyFile ([] :: [Relation])
    unless resume $ runConfigs timeout "down" (reverse st)
    runConfigs timeout "up" noParents


-- Перезапуск заданных дочерних соединений
up timeout resume owner req st hc = do

    -- Проверка что запрашиваемые соединения не имеют других родителей
    liftIO
      $ when (isJust owner)
      $ let conflicts = [ child | (child, parent) <- hc
                                , child `elem` req
                                , parent /= fromJust owner
                        ]
        in unless (null conflicts) $ do
            hPutStrLn stderr "Already have another parents:"
            hPrint stderr conflicts
            exitFailure

    let forDown = do
           n <- req `intersect` st
           n : getDescendants hc n

    liftIO $ do
        save statusFile $ (st \\ forDown) ++ req
        save hierarchyFile $
           if isJust owner
           then hc `union` [ (name, fromJust owner) | name <- req ]
           else (`filter` hc) $
              \ (child, _) -> child `notElem` forDown || child `elem` req

    runConfigs timeout "down" (reverse forDown)
    runConfigs timeout "up" req

import System.Console.GetOpt
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import System.IO (hPutStrLn, stderr)
import System.Exit
import Data.List (intersect)

import Utils.Common
import Utils.Options (commonOptions, getTimeout)
import Utils.Hierarchy (getDescendants)
import Utils.Misc (inEnv, runConfigs, save)


usage = "Usage: netn [OPTION...] config...\n\
        \   or: netu --help"


main :: IO ()
main = inEnv usage commonOptions $ \ opts req st hc
                                  -> new (getTimeout opts) req st hc


new :: Int -> [Name] -> [Name] -> [Relation] -> StateT Bool IO ()
   -- В состоянии — флаг, показывающий была ли ошибка в запуске любого конфига


new _ [] _ _ = liftIO $ do
   hPutStrLn stderr "Required connection names"
   exitFailure


new timeout req st hc = do
   liftIO $ do
      save statusFile req
      save hierarchyFile ([] :: [Relation])
   runConfigs timeout "down" (reverse st)
   runConfigs timeout "up" req

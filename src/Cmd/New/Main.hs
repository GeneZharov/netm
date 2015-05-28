import qualified Data.Set as S
import System.Console.GetOpt
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import System.IO (hPutStrLn, stderr)
import System.Exit

import Utils


usage = "Usage: netn [OPTION...] files..."

opts :: [OptDescr Option]
opts =
  [ Option "q" ["quiet"]   (NoArg Quiet)
     "Подавить вывод stdout"
  , Option "t" ["timeout"] (ReqArg (Timeout . read) "INT")
     "Время ожидания пользовательского скрипта"
  ]


main :: IO ()
main = inEnv usage opts $ \ opts req st
                         -> new (getTimeout opts) st req


new :: Int -> S.Set FilePath -> S.Set FilePath -> StateT Bool IO ()
new timeout st req
   | S.null req = liftIO $ do
       hPutStrLn stderr "Required connection names"
       exitFailure
   | otherwise = do
       liftIO $ saveStatus req
       runConfigs timeout "down" st
       runConfigs timeout "up" req

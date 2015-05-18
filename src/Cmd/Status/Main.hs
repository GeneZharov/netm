import qualified Data.Set as S
import Control.Monad

import Utils


main :: IO ()
main = do
    s <- loadStatus
    forM_ (S.toList s) putStrLn

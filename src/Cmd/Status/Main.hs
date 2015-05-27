import qualified Data.Set as S
import Control.Monad

import Utils


main :: IO ()
main = do
    s <- loadStatus
    mapM_ putStrLn (S.toList s)

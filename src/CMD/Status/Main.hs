import Utils
import qualified Data.Set as S
import Control.Monad


main :: IO ()
main = do
    s <- loadStatus
    forM_ (S.toList s) putStrLn

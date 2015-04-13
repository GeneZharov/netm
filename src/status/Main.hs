import Utils
import qualified Data.Set as S
import Control.Monad


main = do
    status <- loadStatus
    forM_ (S.toList status) putStrLn

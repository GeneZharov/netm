import Control.Monad

import Utils.Misc


main :: IO ()
main = do
    st <- loadStatus
    mapM_ putStrLn st

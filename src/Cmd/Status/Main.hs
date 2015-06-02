import Control.Monad

import Utils


main :: IO ()
main = do
    st <- loadStatus
    mapM_ putStrLn st

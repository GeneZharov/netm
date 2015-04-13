import Utils
import qualified Data.Set as S


main = do

    status <- loadStatus
    new <- argsToFiles

    if S.null new
    then do
        call "down" status
        call "up" status
    else do
        saveStatus (S.union status new)
        call "down" (S.intersection status new)
        call "up" new

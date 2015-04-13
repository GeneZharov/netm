import Utils
import qualified Data.Set as S


main = do
    status <- loadStatus
    new <- argsToFiles

    if S.null new
    then do
        saveStatus S.empty
        call "down" status
    else do
        saveStatus $ S.filter ( \f -> not (S.member f new) ) status
        call "down" new

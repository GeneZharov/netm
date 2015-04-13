import Utils
import qualified Data.Set as S


main = do

    status <- loadStatus
    new <- argsToFiles

    saveStatus new

    call "down" status
    call "up" new

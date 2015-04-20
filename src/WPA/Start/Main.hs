-- Запускает демон wpa_supplicant и проксирует его стандартный вывод пока не 
-- встретит ключевое слово, означающее окончание настройки соединения. После 
-- этого фронтэнд умирает оставляя демон работать фоново.


import System.Environment (getArgs)
import System.Process
import System.IO
import Data.List (isInfixOf)


desired = "CTRL-EVENT-CONNECTED"
    -- Уйти в фоновый режим после того как будет встречена эта подстрока


main :: IO ()
main = do
    args <- getArgs
    (_, o, _, _) <- runInteractiveProcess "wpa_daemon" args Nothing Nothing
    hSetBinaryMode o False
    hSetBuffering o NoBuffering
    showOutput o


showOutput :: Handle -> IO ()
showOutput o = do
    eof <- hIsEOF o
    if eof
    then hPutStrLn stderr "Daemon unexpectally terminated"
    else do
        s <- hGetLine o
        putStrLn s
        if desired `isInfixOf` s
        then putStrLn "Going to background..."
        else showOutput o

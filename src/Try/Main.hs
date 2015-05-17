-- Запускает переданную в аргументах команду, проксирует в неё stdin. Если 
-- переданная команда завершилась неудачей, то сначала распечатает текст 
-- команды, а потом уже распечатает её stdout+stderr. Это позволяет пояснить в 
-- больших скриптах какая именно команда вывела текст и дала сбой.
--
-- stdout и stderr будут распечатаны по отдельности, а не в том порядке, в 
-- котором они выводились. Потому что до завершения команды неизвестно нужно ли 
-- будет предварять вывод заголовком.
--
-- Пример использования:
--   $ try ip address add dev $IFACE $ADDR/$MASK
--   Error 2: ip address add dev wlp3s0 192.168.1.7/24
--   RTNETLINK answers: Cannot assign requested address


import System.Environment (getArgs)
import System.Process
import Text.Printf
import System.Exit
import System.IO
import Data.List (intersperse)


main :: IO ()
main = do

    cmd@(prog:args) <- getArgs

    (_, Just outH, Just errH, p) <-
        createProcess (proc prog args) {
          std_out = CreatePipe
        , std_err = CreatePipe
        }

    code <- waitForProcess p

    case code of
        ExitSuccess -> printOutput outH errH
        code@(ExitFailure code') -> do
            printHeader code' cmd
            printOutput outH errH
            exitWith code


printHeader :: Int -> [String] -> IO ()
printHeader code cmd =
    hPutStrLn stderr $ printf "Error %d: %s" code (unwords cmd)


printOutput :: Handle -> Handle -> IO ()
printOutput outH errH = do
    hPutStr stdout =<< hGetContents outH
    hPutStr stderr =<< hGetContents errH

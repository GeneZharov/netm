-- Фронтэнд к wpa_supplicant. Отправляет его stdout+stderr в syslog.
-- В остальном пытается быть прозрачным:
-- • передаёт собственные аргументы вызова в wpa_supplicant;
-- • stdout+stderr проксируется на собственный stdout;
-- • проксирует код возврата;
--
-- Использовать юниксовую утилиту script вместо собственного терминала очень 
-- неудобно. Она порождает отдельный процесс, в котором и запускается моя 
-- команда. Этот процесс пишет в тот же stdout, что был у оригинального 
-- процесса. Оригинальный процесс в течение 10 сек умирает. Проблема в том, что 
-- я не могу отловить момент завершения второго процесса, так как с не имею его 
-- дескриптора.

{- Команда для отладки:
rg daemon.hs -i wlp3s0 -D nl80211 -c /etc/netm/nest/_wlan/wpa_supplicant.conf
-}


import Control.Monad (forever, forM)
import System.Environment (getArgs)
import System.Process
import Control.Concurrent
import System.IO
import System.Exit
import System.Posix.Terminal
import System.Posix.IO
import Data.List.Split (endBy)

import System.Log
import System.Log.Handler
import System.Log.Handler.Syslog


logStr logH str = emit logH (NOTICE, str) str -- отправляет строку в syslog


main :: IO ()
main = do

    logH <- openlog "wpa_supplicant" [] DAEMON NOTICE

    -- Создание псевдотерминала, через который будет проходить общение с 
    -- wpa_supplicant. Если запустить его не через терминал, то он станет очень 
    -- молчалив. Ключами вернуть дефолтный уровень словесности невозможно.
    (master, slave) <- openPseudoTerminal
    hslave <- fdToHandle slave
    hSetBuffering hslave NoBuffering

    -- Запуск wpa_supplicant
    args <- getArgs
    (_, _, _, p) <- createProcess (proc "wpa_supplicant" args) {
        env = Just [ ("TERM", "xterm") ]
      , std_in  = UseHandle hslave
      , std_out = UseHandle hslave
      , std_err = UseHandle hslave
      , close_fds = True
      , delegate_ctlc = True
      }

    -- Читаю вывод wpa_supplicant из созданного псевдотерминала
    forkIO $ forever $ do
        (s, _) <- fdRead master 1024
        let ls = endBy "\n" . filter (/='\r') $ s
        forM ls $ \ l -> do
            putStrLn l >> hFlush stdout
            logStr logH l

    exitWith =<< waitForProcess p

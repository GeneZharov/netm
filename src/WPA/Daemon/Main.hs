-- Использовать юниксовую утилиту script вместо собственного терминала очень 
-- неудобно. Она порождает отдельный процесс, в котором и запускается моя 
-- команда. Этот процесс пишет в тот же stdout, что был у оригинального 
-- процесса. Оригинальный процесс в течение 10 сек умирает. Проблема в том, что 
-- я не могу отловить момент завершения второго процесса, так как с не имею его 
-- дескриптора.

{- Команда для отладки:
rg daemon.hs -i wlp3s0 -D nl80211 -c /etc/netm/nest/_wlan/wpa_supplicant.conf
-}


import Control.Monad (forever, unless)
import System.Environment (getArgs)
import System.Process
import Control.Concurrent
import System.IO
import System.Exit
import System.Posix.IO
import System.Posix.Signals

import System.Log
import System.Log.Handler
import System.Log.Handler.Syslog


logStr logH str = emit logH (NOTICE, str) str -- отправляет строку в syslog


main :: IO ()
main = do

   logH <- openlog "wpa_supplicant" [] DAEMON NOTICE

   -- Запуск wpa_supplicant
   args <- getArgs
   (_, Just out, _, p) <- createProcess (proc "wpa_supplicant" args)
      { std_out = CreatePipe
      , close_fds = True
      , delegate_ctlc = True
      }

   -- stdout
   hSetBuffering stdout LineBuffering
      -- Как только считана новая строка — она должна быть выведена
   forkIO $ forever $ do
      l <- hGetLine out
      logStr logH l
      closed <- hIsClosed stdout
      unless closed (putStrLn l)

   installHandler sigUSR1 (CatchOnce daemonize) Nothing
   exitWith =<< waitForProcess p


daemonize :: IO ()
daemonize = do
   hClose stdout
   hClose stderr

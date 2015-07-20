import System.Environment (getArgs)
import System.Process
import System.Process.Internals
import System.IO
import System.Exit (exitFailure)
import System.Posix.Signals
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Control.Monad (unless, liftM)


-- Returns Just pid or Nothing if process has already exited
getPID :: ProcessHandle -> IO (Maybe PHANDLE)
getPID ph = withProcessHandle ph getPID'
   where getPID' :: ProcessHandle__ -> IO (Maybe PHANDLE)
         getPID' (OpenHandle x)   = return (Just x)
         getPID' (ClosedHandle _) = return Nothing


desired = "CTRL-EVENT-CONNECTED"
   -- Уйти в фоновый режим после того как будет встречена эта подстрока


main :: IO ()
main = do
   args <- getArgs
   let (quiet, args') = if head args == "--quiet"
                        then (True, tail args)
                        else (False, args)
   (_, Just out, _, p) <-
      createProcess (proc "wpa_daemon" args')
                          { create_group  = True
                          , delegate_ctlc = True
                          , std_out = CreatePipe
                          }
   hSetBinaryMode out False
   hSetBuffering out NoBuffering
   parseLine quiet p out


parseLine :: Bool -> ProcessHandle -> Handle -> IO ()
parseLine quiet p out = do
   eof <- hIsEOF out
   if eof
   then do
      hPutStrLn stderr "Daemon terminated before it was daemonized"
      exitFailure
   else do
      s <- hGetLine out
      unless quiet (putStrLn s)
      if desired `isInfixOf` s
      then do
         signalProcess sigUSR1 =<< (fromJust `liftM` getPID p)
         putStrLn "Going to background"
      else parseLine quiet p out

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
                        , std_out = CreatePipe
                        }

   Just pid <- getPID p
   pid `terminateBy` [sigTERM, sigINT, sigQUIT]

   hSetBinaryMode out False
   hSetBuffering out NoBuffering
   parseLine quiet pid out


terminateBy :: PHANDLE -> [Signal] -> IO ()
terminateBy pid = mapM_ $ \ sig -> installHandler sig handler Nothing
   where handler = Catch (signalProcess sigTERM pid)


parseLine :: Bool -> PHANDLE -> Handle -> IO ()
parseLine quiet pid out = do
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
         signalProcess sigUSR1 pid -- просьба демонизироваться
         putStrLn "Going to background"
      else parseLine quiet pid out

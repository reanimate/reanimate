module Reanimate.Driver.Daemon where

import           Control.Concurrent
import           Control.Exception         as E
import           Control.Monad
import qualified Data.ByteString.Char8     as BS
import           Network.Socket
import           Network.Socket.ByteString
import qualified Reanimate.Driver.Server   as Server
import           System.FSNotify
import           System.FilePath
import           System.Environment

import Detach

{-
Main run message:

  Reanimate has gone into daemon mode and will block until you hit
  ctrl-c. While Reanimate is in daemon mode, you can open a new
  console or terminal and execute your animation code again. It'll
  automatically send the new animation to the browser window.

  Linux users can pass --daemon to reanimate to run the process in
  the background. Windows users have to use PowerShell and explicitly
  fork the process:
    Start-Process -NoNewWindow ./reanimate_exe

  Connection to the browser window will be lost if you hit ctrl-c.


Executing with daemon:
  Send animation to daemon and exit.
Executing without daemon:
  Run daemon locally.
  Render animation once.
  Wait without exiting.
  Detach if given --daemon flag. Only for Linux.

Executing on Linux:
  Running 'main' will start the daemon if it isn't already running.
  Then it'll render the animation and send it to the daemon.
  The daemon will open a browser window.
  Daemon stops after 30 minutes of inactivity.

Executing on Windows:
  'main' will act as daemon and not return.
  Powershell command for running in the background:
    Start-Process -NoNewWindow ./reanimate_exe

  Subsequent runs will send animation to daemon and quit.

In GHCi:
  Start local daemon thread if necessary.
  :cmd reanimateLive
    1. wait for changes
    2. ":r"
    3. ":main"
    4. ":cmd Reanimate.reanimateLive"



Web port: 9161
Daemon port: 9162?

-}

{-
  Improvements over previous infrastructure:
  - Multiple browser windows can be open.
  - Browser window won't get stuck trying to open a connection.
  - GHCi is robust and works with both cabal and stack.
  - Refreshing the code will re-open closed browser windows.
-}

-- | Load a reanimate program in GHCi and make sure 'main' is available.
--   Then run:
-- @
-- :cmd reanimateLive
-- @
--
-- This works by sending the commands ':r' and ':main' to your GHCi instance
-- when any source file is changed.
reanimateLive :: IO String
reanimateLive = do
  void ensureDaemon
  args <- getArgs
  case args of
    ["primed"] -> waitForChanges
    _ -> return ()
  return $ unlines
        [ ":r"
        , ":main"
        , ":cmd System.Environment.withArgs [\"primed\"] Reanimate.reanimateLive" ]

-- | Load an animation in GHCi. Anything of type 'Animation' can be live reloaded.
--
-- @
-- :cmd reanimateLiveEntry "drawCircle"
-- @
--
-- This works by sending the commands ':r' and 'Reanimate.reanimate {entry}' to your
-- GHCi instance when any source file is changed.
reanimateLiveEntry :: String -> IO String
reanimateLiveEntry animation = do
  void ensureDaemon
  args <- getArgs
  case args of
    ["primed"] -> waitForChanges
    _ -> return ()
  return $ unlines
        [ ":r"
        , "Reanimate.reanimate (" ++ animation ++ ")"
        , ":cmd System.Environment.withArgs [\"primed\"] (Reanimate.reanimateLiveEntry " ++ show animation ++ ")"]

waitForChanges :: IO ()
waitForChanges = withManager $ \mgr -> do
    lock <- newEmptyMVar
    stop <- watchTree mgr "." check (const $ putMVar lock ())
    takeMVar lock
    stop
  where
    check event =
      takeExtension (eventPath event) `elem` sourceExtensions ||
      takeExtension (eventPath event) `elem` dataExtensions
    sourceExtensions = [".hs", ".lhs"]
    dataExtensions = [".jpg", ".png", ".bmp", ".pov", ".tex", ".csv"]
  

data DaemonCommand
  = DaemonCount Int
  | DaemonFrame Int FilePath
  | DaemonStop
  deriving (Show)

sendCommand :: DaemonCommand -> IO ()
sendCommand cmd = withSocketsDo $ handle (\SomeException{} -> return ()) $ do
  addr <- resolve
  E.bracket (open addr) close $ \sock -> do
    void $ send sock $ case cmd of
      DaemonCount count    -> BS.pack $ unwords ["frame_count", show count]
      DaemonFrame nth path -> BS.pack $ unwords ["frame", show nth, path]
      DaemonStop           -> BS.pack $ unwords ["stop"]
    return ()
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "9162")
    open addr = E.bracketOnError (oSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock

hasDaemon :: IO Bool
hasDaemon = withSocketsDo $ handle (\SomeException{} -> return False) $ do
  addr <- resolve
  E.bracket (open addr) close (const $ return True)
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "9162")
    open addr = E.bracketOnError (oSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock

oSocket :: AddrInfo -> IO Socket
oSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

ensureDaemon :: IO Bool
ensureDaemon = do
  daemon <- hasDaemon
  if daemon
    then pure True
    else localDaemon

killDaemon :: IO ()
killDaemon = sendCommand DaemonStop

localDaemon :: IO Bool
localDaemon = do
  killDaemon
  detach Server.daemon


module Reanimate.Driver ( reanimate ) where

import           Control.Concurrent (MVar, forkIO, killThread, modifyMVar_,
                                     newEmptyMVar, putMVar, takeMVar)
import           Control.Exception  (finally)
import           Control.Monad.Fix  (fix)
import qualified Data.Text          as T
import qualified Data.Text.Read     as T
import           Network.WebSockets
import           System.Directory   (findFile, listDirectory)
import           System.Environment (getArgs, getProgName)
import           System.FilePath
import           System.FSNotify
import System.Exit
import           System.IO          (BufferMode (..), hPutStrLn, hSetBuffering,
                                     stderr, stdin)

import           Data.Maybe
import           Paths_reanimate
import           Reanimate.Misc     (runCmdLazy, runCmd_, withTempDir,
                                     withTempFile)
import           Reanimate.Monad    (Animation)
import           Reanimate.Render   (render, renderSvgs)
import           Web.Browser        (openBrowser)

opts = defaultConnectionOptions
  { connectionCompressionOptions = PermessageDeflateCompression defaultPermessageDeflate }

reanimate :: Animation -> IO ()
reanimate animation = do
  watch <- startManager
  args <- getArgs
  hSetBuffering stdin NoBuffering
  case args of
    ["once"] -> renderSvgs animation
    ["render", target] ->
      render animation target
    _ -> withTempDir $ \tmpDir -> do
      url <- getDataFileName "viewer/build/index.html"
      putStrLn "Opening browser..."
      bSucc <- openBrowser url
      if bSucc
          then putStrLn "Browser opened."
          else hPutStrLn stderr $ "Failed to open browser. Manually visit: " ++ url
      runServerWith "127.0.0.1" 9161 opts $ \pending -> do
        putStrLn "Server pending..."
        prog <- getProgName
        lst <- listDirectory "."
        mbSelf <- findFile ("." : lst) prog
        blocker <- newEmptyMVar :: IO (MVar ())
        case mbSelf of
          Nothing -> do
            hPutStrLn stderr "Failed to find own source code."
          Just self -> do
            conn <- acceptRequest pending
            slave <- newEmptyMVar
            let handler = modifyMVar_ slave $ \tid -> do
                  sendTextData conn (T.pack "Compiling")
                  putStrLn "Killing and respawning..."
                  killThread tid
                  tid <- forkIO $ slaveHandler conn self tmpDir
                  return tid
                killSlave = do
                  tid <- takeMVar slave
                  killThread tid
            putStrLn "Found self. Listening..."
            stop <- watchFile watch self handler
            putMVar slave =<< forkIO (return ())
            let loop = do
                  fps <- receiveData conn :: IO T.Text
                  handler
                  loop
            loop `finally` (killSlave >> stop)

slaveHandler conn self tmpDir = withTempFile ".exe" $ \tmpExecutable -> do
  ret <- runCmd_ "stack" $ ["ghc", "--"] ++ ghcOptions tmpDir ++ [self, "-o", tmpExecutable]
  case ret of
    Left err ->
      sendTextData conn $ T.pack $ "Error" ++ unlines (drop 3 (lines err))
    Right{} -> do
      getFrame <- runCmdLazy tmpExecutable ["once", "+RTS", "-N", "-M1G", "-RTS"]
      (frameCount,_) <- expectFrame =<< getFrame
      -- sendTextData conn (T.pack "Compiled")
      sendTextData conn (T.pack $ show frameCount)
      fix $ \loop -> do
        (frameIdx, frame) <- expectFrame =<< getFrame
        sendTextData conn (T.pack $ show frameIdx)
        sendTextData conn frame
        loop
  where
    expectFrame (Left "") = do
      sendTextData conn (T.pack "Done")
      exitWith ExitSuccess
    expectFrame (Left err) = do
      sendTextData conn $ T.pack $ "Error" ++ err
      exitWith (ExitFailure 1)
    expectFrame (Right frame) =
      case T.decimal frame of
        Left err -> do
          hPutStrLn stderr (T.unpack frame)
          hPutStrLn stderr $ "expectFrame: " ++ err
          sendTextData conn $ T.pack $ "Error" ++ err
          exitWith (ExitFailure 1)
        Right (frameNumber, rest) -> pure (frameNumber, rest)

watchFile watch file action = watchDir watch (takeDirectory file) check (const action)
  where
    check event = takeFileName (eventPath event) == takeFileName file

ghcOptions :: FilePath -> [String]
ghcOptions tmpDir =
    ["-rtsopts", "--make", "-threaded", "-O2"] ++
    ["-odir", tmpDir, "-hidir", tmpDir]

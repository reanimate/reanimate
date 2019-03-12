module Reanimate.Driver ( reanimate ) where

import           Control.Concurrent (MVar, forkIO, killThread, modifyMVar_,
                                     newEmptyMVar, putMVar)
import           Control.Exception  (finally)
import           Control.Monad.Fix  (fix)
import qualified Data.Text          as T
import           Network.WebSockets
import           System.Directory   (findExecutable, findFile, listDirectory)
import           System.Environment (getArgs, getProgName)
import           System.FilePath
import           System.FSNotify
import           System.IO          (BufferMode (..), hPutStrLn, hSetBuffering,
                                     stderr, stdin)

import           Data.Maybe
import           Paths_reanimate
import           Reanimate.Misc     (runCmd, runCmdLazy, runCmd_, withTempDir,
                                     withTempFile)
import           Reanimate.Monad    (Animation)
import           Reanimate.Render   (renderSvgs)

opts = defaultConnectionOptions
  { connectionCompressionOptions = PermessageDeflateCompression defaultPermessageDeflate }

reanimate :: Animation -> IO ()
reanimate animation = do
  watch <- startManager
  args <- getArgs
  hSetBuffering stdin NoBuffering
  case args of
    ["once"] -> renderSvgs animation
    _ -> withTempDir $ \tmpDir -> do
      url <- getDataFileName "viewer/build/index.html"
      openBrowser url
      runServerWith "127.0.0.1" 9161 opts $ \pending -> do
        putStrLn "Server pending."
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
                  putStrLn "Kill and respawn."
                  killThread tid
                  tid <- forkIO $ withTempFile ".exe" $ \tmpExecutable -> do
                    ret <- runCmd_ "stack" $ ["ghc", "--"] ++ ghcOptions tmpDir ++ [self, "-o", tmpExecutable]
                    case ret of
                      Left err ->
                        sendTextData conn $ T.pack $ "Error" ++ unlines (drop 3 (lines err))
                      Right{} -> do
                        getFrame <- runCmdLazy tmpExecutable ["once", "+RTS", "-N", "-M200M", "-RTS"]
                        flip fix [] $ \loop acc -> do
                          frame <- getFrame
                          case frame of
                            Left "" -> do
                              sendTextData conn (T.pack "Done")
                              -- insertCache msg (reverse acc)
                            Left err -> do
                              -- _ <- getChanContents queue
                              sendTextData conn $ T.pack $ "Error" ++ err
                            Right frame -> do
                              sendTextData conn frame
                              loop (frame : acc)
                  return tid
            putStrLn "Found self. Listening."
            stop <- watchFile watch self handler
            putMVar slave =<< forkIO (return ())
            let loop = do
                  fps <- receiveData conn :: IO T.Text
                  handler
                  loop
            loop `finally` stop

watchFile watch file action = watchDir watch (takeDirectory file) check (const action)
  where
    check event = takeFileName (eventPath event) == takeFileName file

ghcOptions :: FilePath -> [String]
ghcOptions tmpDir =
    ["-rtsopts", "--make", "-threaded", "-O2"] ++
    ["-odir", tmpDir, "-hidir", tmpDir]

openBrowser :: String -> IO ()
openBrowser url = do
  xdgOpen <- findExecutable "xdg-open"
  open <- findExecutable "open"
  case listToMaybe (catMaybes [xdgOpen, open]) of
    Nothing ->
      hPutStrLn stderr $ "Failed to open browser. Manually visit: " ++ url
    Just prog ->
      runCmd prog [url]

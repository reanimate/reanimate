{-# LANGUAGE ScopedTypeVariables #-}
module Reanimate.Driver.Server
  ( serve
  , findOwnSource
  ) where

import           Control.Concurrent      (forkIO, killThread, modifyMVar_,
                                          newEmptyMVar, putMVar, takeMVar,
                                          threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception       (SomeException, catch, finally)
import           Control.Monad
import           Control.Monad.Fix       (fix)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Read          as T
import           GHC.Environment         (getFullArgs)
import           Network.WebSockets
import           Paths_reanimate
import           Reanimate.Misc          (runCmdLazy, runCmd_)
import           System.Directory        (doesFileExist, findFile,
                                          listDirectory, makeAbsolute,
                                          withCurrentDirectory)
import           System.Environment      (getProgName)
import           System.Exit
import           System.FilePath
import           System.FSNotify
import           System.IO
import           System.IO.Temp
import           Web.Browser             (openBrowser)

opts :: ConnectionOptions
opts = defaultConnectionOptions
  { connectionCompressionOptions = PermessageDeflateCompression defaultPermessageDeflate }

serve :: IO ()
serve = withManager $ \watch -> do
  hSetBuffering stdin NoBuffering
  self <- findOwnSource
  hasConnectionVar <- newMVar False

  -- There might already browser window open. Wait 2s to see if that window
  -- connects to us. If not, open a new window.
  _ <- forkIO $ do
    threadDelay (2*10^(6::Int))
    hasConn <- readMVar hasConnectionVar
    unless hasConn openViewer

  putStrLn "Listening..."
  runServerWith "127.0.0.1" 9161 opts $ \pending -> do
    putStrLn "New connection received."
    hasConn <- swapMVar hasConnectionVar True
    if hasConn
      then do
        putStrLn "Already connected to browser. Rejecting."
        rejectRequestWith pending defaultRejectRequest
      else do
        conn <- acceptRequest pending
        slave <- newEmptyMVar
        let handler = modifyMVar_ slave $ \tid -> do
              putStrLn "Reloading code..."
              killThread tid
              forkIO $ ignoreErrors $ slaveHandler conn self
            killSlave = do
              tid <- takeMVar slave
              killThread tid
        stop <- watchFile watch self handler
        putMVar slave =<< forkIO (return ())
        let loop = do
              -- FIXME: We don't use fps here.
              _fps <- receiveData conn :: IO T.Text
              handler
              loop
        loop `finally` (swapMVar hasConnectionVar False >> stop >> killSlave)

ignoreErrors :: IO () -> IO ()
ignoreErrors action = action `catch` \(_::SomeException) -> return ()

openViewer :: IO ()
openViewer = do
  url <- getDataFileName "viewer/build/index.html"
  putStrLn "Opening browser..."
  bSucc <- openBrowser url
  if bSucc
      then putStrLn "Browser opened."
      else hPutStrLn stderr $ "Failed to open browser. Manually visit: " ++ url

slaveHandler :: Connection -> FilePath -> IO ()
slaveHandler conn self =
  withCurrentDirectory (takeDirectory self) $
  withSystemTempDirectory "reanimate" $ \tmpDir ->
  withTempFile tmpDir "reanimate.exe" $ \tmpExecutable handle -> do
    hClose handle
    sendTextData conn (T.pack "Compiling")
    ret <- runCmd_ "stack" $ ["ghc", "--"] ++ ghcOptions tmpDir ++ [takeFileName self, "-o", tmpExecutable]
    case ret of
      Left err ->
        sendTextData conn $ T.pack $ "Error" ++ unlines (drop 3 (lines err))
      Right{} -> do
        getFrame <- runCmdLazy tmpExecutable ["raw", "+RTS", "-N", "-M1G", "-RTS"]
        (frameCount,_) <- expectFrame =<< getFrame
        sendTextData conn (T.pack $ show frameCount)
        fix $ \loop -> do
          (frameIdx, frame) <- expectFrame =<< getFrame
          sendTextData conn (T.pack $ show frameIdx)
          sendTextData conn frame
          loop
  where
    expectFrame :: Either String Text -> IO (Integer, Text)
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

watchFile :: WatchManager -> FilePath -> IO () -> IO StopListening
watchFile watch file action = watchTree watch (takeDirectory file) check (const action)
  where
    check event =
      takeFileName (eventPath event) == takeFileName file ||
      takeExtension (eventPath event) `elem` sourceExtensions ||
      takeExtension (eventPath event) `elem` dataExtensions
    sourceExtensions = [".hs", ".lhs"]
    dataExtensions = [".jpg", ".png", ".bmp", ".pov", ".tex", ".csv"]

ghcOptions :: FilePath -> [String]
ghcOptions tmpDir =
    ["-rtsopts", "--make", "-threaded", "-O2"] ++
    ["-odir", tmpDir, "-hidir", tmpDir]

-- FIXME: Move to a different module
-- FIXME: Gracefully disable code reloading if source is missing.
findOwnSource :: IO FilePath
findOwnSource = do
  fullArgs <- getFullArgs
  stackSource <- makeAbsolute (last fullArgs)
  exist <- doesFileExist stackSource
  if exist
    then return stackSource
    else do
      prog <- getProgName
      lst <- listDirectory "."
      mbSelf <- findFile ("." : lst) prog
      case mbSelf of
        Nothing -> do
          hPutStrLn stderr "Failed to find own source code."
          exitFailure
        Just self -> pure self

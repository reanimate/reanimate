{-# LANGUAGE ScopedTypeVariables #-}
module Reanimate.Driver.Server
  ( serve
  , findOwnSource
  ) where

import           Control.Concurrent
import           Control.Exception       (SomeException, catch, finally)
import           Control.Monad
import           Control.Monad.Fix       (fix)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Data.Text.Read          as T
import           GHC.Environment         (getFullArgs)
import           Network.WebSockets
import           Paths_reanimate
import           Reanimate.Misc          (runCmdLazy, runCmd_)
import           System.Directory        (createDirectoryIfMissing,
                                          doesFileExist, findFile,
                                          listDirectory, makeAbsolute,
                                          removeDirectoryRecursive,
                                          withCurrentDirectory)
import           System.Environment      (getProgName)
import           System.Exit
import           System.Process
import           System.FilePath
import           System.FSNotify
import           System.IO
import           System.IO.Temp
import           Web.Browser             (openBrowser)

opts :: ConnectionOptions
opts = defaultConnectionOptions
  { connectionCompressionOptions = PermessageDeflateCompression defaultPermessageDeflate }

serve :: Bool -> Maybe FilePath -> Maybe FilePath -> IO ()
serve verbose mbGHCPath mbSelfPath = withManager $ \watch -> do
  hSetBuffering stdin NoBuffering
  self <- case mbSelfPath of
    Nothing   -> findOwnSource
    Just path -> pure path
  hasConnectionVar <- newMVar False

  -- There might already browser window open. Wait 2s to see if that window
  -- connects to us. If not, open a new window.
  _ <- forkIO $ do
    threadDelay (2*10^(6::Int))
    hasConn <- readMVar hasConnectionVar
    unless hasConn openViewer

  putStrLn "Listening..."
  let options = ServerOptions
        { serverHost = "127.0.0.1"
        , serverPort = 9161
        , serverConnectionOptions = opts
        , serverRequirePong = Nothing }
  runServerWithOptions options $ \pending -> do
    putStrLn "New connection received."
    hasConn <- swapMVar hasConnectionVar True
    if hasConn
      then do
        putStrLn "Already connected to browser. Rejecting."
        rejectRequestWith pending defaultRejectRequest
      else withSystemTempDirectory "reanimate-svgs" $ \tmpDir -> do
        createDirectoryIfMissing True tmpDir
        conn <- acceptRequest pending
        slave <- newEmptyMVar
        let handler = modifyMVar_ slave $ \tid -> do
              putStrLn "Reloading code..."
              killThread tid
              forkIO $ ignoreErrors $ slaveHandler verbose mbGHCPath conn self tmpDir
            killSlave = do
              tid <- takeMVar slave
              killThread tid
        stop <- watchFile watch self handler
        putMVar slave =<< forkIO (return ())
        handler
        let loop = do
              -- FIXME: We don't use msg here.
              _msg <- receiveData conn :: IO T.Text
              handler
              loop
        loop `finally` (removeDirectoryRecursive tmpDir >> swapMVar hasConnectionVar False >> stop >> killSlave)

ignoreErrors :: IO () -> IO ()
ignoreErrors action = action `catch` \(_::SomeException) -> return ()

openViewer :: IO ()
openViewer = do
  url <- getDataFileName "viewer-elm/dist/index.html"
  putStrLn "Opening browser..."
  bSucc <- openBrowser url
  if bSucc
      then putStrLn "Browser opened."
      else hPutStrLn stderr $ "Failed to open browser. Manually visit: " ++ url

slaveHandler :: Bool -> Maybe FilePath -> Connection -> FilePath -> FilePath -> IO ()
slaveHandler verbose mbGHCPath conn self svgDir =
  withCurrentDirectory (takeDirectory self) $
  withSystemTempDirectory "reanimate" $ \tmpDir ->
  withTempFile tmpDir "reanimate.exe" $ \tmpExecutable handle -> do
    -- cap <- getNumCapabilities
    let n = 25
    sem <- newQSemN n
    hClose handle
    lock <- newMVar ()
    sendTextData conn (T.pack "status\nCompiling")
    ret <- case mbGHCPath of
      Nothing -> do
        when verbose $
          putStrLn $ "Running: " ++ showCommandForUser "stack"
            (["ghc", "--"] ++ ghcOptions tmpDir ++ [takeFileName self, "-o", tmpExecutable])
        runCmd_ "stack" $ ["ghc", "--"] ++ ghcOptions tmpDir ++ [takeFileName self, "-o", tmpExecutable]
      Just ghc -> do
        when verbose $
          putStrLn $ "Running: " ++ showCommandForUser ghc
            (ghcOptions tmpDir ++ [takeFileName self, "-o", tmpExecutable])
        runCmd_ ghc $ ghcOptions tmpDir ++ [takeFileName self, "-o", tmpExecutable]
    case ret of
      Left err ->
        sendTextData conn $ T.pack $ "error\n" ++ unlines (drop 3 (lines err))
      Right{} -> runCmdLazy tmpExecutable execOpts $ \getFrame -> do
        (frameCount,_) <- expectFrame sem =<< getFrame
        sendTextData conn (T.pack $ "frame_count\n" ++ show frameCount)
        fix $ \loop -> do
          (frameIdx, frame) <- expectFrame sem =<< getFrame
          -- putStrLn $ "Got frame: " ++ show frameIdx
          let fileName = svgDir </> takeBaseName tmpExecutable <.> show frameIdx <.> "svg"
              -- pngName = replaceExtension fileName "png"
          _ <- forkIO $ do
            waitQSemN sem 1
            T.writeFile fileName frame
            -- runCmd "rsvg-convert"
            --   [ fileName
            --   , "--width=256" -- "--width=1024"
            --   , "--height=144" -- "--height=576"
            --   , "--output", pngName ]
            withMVar lock $ \_ ->
              sendTextData conn (T.pack $ "frame\n" ++ show frameIdx ++ "\n" ++ fileName)
            signalQSemN sem 1
          loop
  where
    execOpts = ["raw", "+RTS", "-N", "-M2G", "-RTS"]
    expectFrame :: QSemN -> Either String Text -> IO (Integer, Text)
    expectFrame sem (Left "") = do
      waitQSemN sem 25 -- =<< getNumCapabilities
      sendTextData conn (T.pack "status\nDone")
      exitSuccess
    expectFrame _ (Left err) = do
      sendTextData conn $ T.pack $ "Error" ++ err
      exitWith (ExitFailure 1)
    expectFrame _ (Right frame) =
      case T.decimal frame of
        Left err -> do
          hPutStrLn stderr (T.unpack frame)
          hPutStrLn stderr $ "expectFrame: " ++ err
          sendTextData conn $ T.pack $ "error\n" ++ err
          exitWith (ExitFailure 1)
        Right (frameNumber, rest) ->
          pure (frameNumber, rest)

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

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reanimate.Driver.Server
  ( serve
  , findOwnSource
  ) where

import           Control.Concurrent
import           Control.Exception      (SomeException, catch, finally)
import           Control.Monad
import           Data.IORef
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Read         as T
import           Data.Time
import           GHC.Environment        (getFullArgs)
import           Language.Haskell.Ghcid
import           Network.WebSockets
import           Paths_reanimate
import           Reanimate.Misc         (runCmdLazy, runCmd_)
import           System.Directory       (createDirectoryIfMissing,
                                         doesFileExist, findFile, listDirectory,
                                         makeAbsolute,
                                         withCurrentDirectory)
import           System.Environment     (getProgName)
import           System.Exit
import           System.FilePath
import           System.FSNotify
import           System.IO
import           System.IO.Temp
import           System.Process
import           Web.Browser            (openBrowser)

opts :: ConnectionOptions
opts = defaultConnectionOptions
  { connectionCompressionOptions = PermessageDeflateCompression defaultPermessageDeflate }

serve :: Bool -> Maybe FilePath -> [String] -> Maybe FilePath -> IO ()
serve verbose mbGHCPath extraGHCOpts mbSelfPath = withManager $ \watch -> do
  hSetBuffering stdin NoBuffering
  self <- maybe requireOwnSource pure mbSelfPath
  when verbose $
    logMsg $ "Found own source code at: " ++ self
  hasConnectionVar <- newMVar False

  ghci <- ghciBackend mbGHCPath self

  -- There might already browser window open. Wait 2s to see if that window
  -- connects to us. If not, open a new window.
  _ <- forkIO $ do
    threadDelay (2*10^(6::Int))
    hasConn <- readMVar hasConnectionVar
    unless hasConn openViewer
  logMsg "Listening..."
  let options = ServerOptions
        { serverHost = "127.0.0.1"
        , serverPort = 9161
        , serverConnectionOptions = opts
        , serverRequirePong = Nothing }
  withSystemTempDirectory "reanimate-svgs" $ \tmpDir ->
    runServerWithOptions options $ \pending -> do
      logMsg "New connection received."
      hasConn <- swapMVar hasConnectionVar True
      if hasConn
        then do
          logMsg "Already connected to browser. Rejecting."
          rejectRequestWith pending defaultRejectRequest
        else do
          createDirectoryIfMissing True tmpDir
          conn <- acceptRequest pending
          slave <- newEmptyMVar
          let handler = modifyMVar_ slave $ \tid -> do
                logMsg "Reloading code..."
                killThread tid
                forkIO $ ignoreErrors $ slaveHandler verbose mbGHCPath extraGHCOpts conn ghci self tmpDir
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
              cleanup = do
                stop
                killSlave
                _ <- swapMVar hasConnectionVar False
                return ()
          loop `finally` cleanup

ignoreErrors :: IO () -> IO ()
ignoreErrors action = action `catch` \(_::SomeException) -> return ()

openViewer :: IO ()
openViewer = do
  url <- getDataFileName "viewer-elm/dist/index.html"
  logMsg "Opening browser..."
  bSucc <- openBrowser url
  if bSucc
      then logMsg "Browser opened."
      else hPutStrLn stderr $ "Failed to open browser. Manually visit: " ++ url

slaveHandler :: Bool -> Maybe FilePath -> [String] -> Connection -> GhciBackend
             -> FilePath -> FilePath -> IO ()
slaveHandler verbose mbGHCPath extraGHCOpts conn ghci self svgDir =
  withCurrentDirectory (takeDirectory self) $
  withSystemTempDirectory "reanimate" $ \tmpDir ->
  withTempFile tmpDir "reanimate.exe" $ \tmpExecutable handle -> do
    outputFolder <- createTempDirectory svgDir "svgs"
    let frameFileName frameIdx =
          outputFolder </> show frameIdx <.> "svg"

    sentFrameCount <- newMVar False
    hClose handle
    lock <- newMVar ()
    sendWebMessage conn $ WebStatus "Compiling"
    ghciThread <- forkIO $ do
      firstFrame <- newIORef True
      ghciReload ghci
      logMsg "GHCi reload done."
      ghciGenerate ghci outputFolder $ \frameIdx -> do
        first <- readIORef firstFrame
        writeIORef firstFrame False
        if first
          then
            modifyMVar_ sentFrameCount $ \sent -> do
              unless sent $
                sendWebMessage conn $ WebFrameCount frameIdx
              logMsg "Framecount sent."
              return True
          else
            withMVar lock $ \_ ->
              sendWebMessage conn $ WebFrame frameIdx (frameFileName frameIdx)
      logMsg "GHCi render done."
    ret <- case mbGHCPath of
      Nothing -> do
        let args = ["ghc", "--"] ++ ghcOptions tmpDir ++ extraGHCOpts ++ [takeFileName self, "-o", tmpExecutable]
        when verbose $
          logMsg $ "Running: " ++ showCommandForUser "stack" args
        runCmd_ "stack" args
      Just ghc -> do
        let args = ghcOptions tmpDir ++ extraGHCOpts ++ [takeFileName self, "-o", tmpExecutable]
        when verbose $
          logMsg $ "Running: " ++ showCommandForUser ghc args
        runCmd_ ghc args
    logMsg "Compile done."
    case ret of
      Left err ->
        sendWebMessage conn $ WebError $ unlines (lines err)
      Right{} -> runCmdLazy tmpExecutable (execOpts outputFolder) $ \getFrame -> do
        frameCount <- expectFrame =<< getFrame
        modifyMVar_ sentFrameCount $ \sent -> do
          unless sent $
            sendWebMessage conn $ WebFrameCount frameCount
          return True
        replicateM_ frameCount $ do
          frameIdx <- expectFrame =<< getFrame
          withMVar lock $ \_ ->
            sendWebMessage conn $ WebFrame frameIdx (frameFileName frameIdx)
        logMsg "Optimized render done."
        killThread ghciThread
  where
    execOpts output =
      [ "raw", "--output", output, "--offset", "1"
      , "+RTS", "-N", "-M2G", "-RTS"]
    expectFrame :: Either String Text -> IO Int
    expectFrame (Left "") = do
      sendWebMessage conn $ WebStatus "Done"
      exitSuccess
    expectFrame (Left err) = do
      sendWebMessage conn $ WebError err
      exitWith (ExitFailure 1)
    expectFrame (Right frame) =
      case T.decimal frame of
        Left err -> do
          hPutStrLn stderr (T.unpack frame)
          raiseError conn err
        Right (frameNumber, "") ->
          pure frameNumber
        Right {} -> do
          let err = "Unexpected output"
          hPutStrLn stderr (T.unpack frame)
          raiseError conn err

raiseError :: Connection -> String -> IO a
raiseError conn err = do
  hPutStrLn stderr $ "expectFrame: " ++ err
  sendWebMessage conn $ WebError err
  exitWith (ExitFailure 1)

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
requireOwnSource :: IO FilePath
requireOwnSource = do
  mbSelf <- findOwnSource
  case mbSelf of
    Nothing -> do
      hPutStrLn stderr
        "Rendering in browser window is only available when interpreting.\n\
        \To render a video file, use the 'render' command or run again with --help\n\
        \to see all available options."
      exitFailure
    Just self -> pure self

findOwnSource :: IO (Maybe FilePath)
findOwnSource = do
  fullArgs <- getFullArgs
  stackSource <- makeAbsolute (last fullArgs)
  exist <- doesFileExist stackSource
  if exist && isHaskellFile stackSource
    then return (Just stackSource)
    else do
      prog <- getProgName
      let hsProg
            | isHaskellFile prog = prog
            | otherwise = replaceExtension prog "hs"
      lst <- listDirectory "."
      findFile ("." : lst) hsProg

isHaskellFile :: FilePath -> Bool
isHaskellFile path = takeExtension path `elem` [".hs", ".lhs"]

logMsg :: String -> IO ()
logMsg msg = do
    now <- getCurrentTime
    putStrLn $ formatTime defaultTimeLocale fmt now ++ ": " ++ msg
  where
    fmt = "%F %T%2Q"

-------------------------------------------------------------------------------
-- Ghci interface

-- stack
-- cabal
-- raw
-- none?
newtype GhciBackend = GhciBackend (MVar Ghci)

ghciBackend :: Maybe FilePath -> FilePath -> IO GhciBackend
ghciBackend mbGHCPath self = do
  let ghciProc =
        case mbGHCPath of
          Just ghcPath ->
            proc ghcPath $ ["--interactive", "+RTS"] ++ words memoryLimit ++ ["-RTS"]
          Nothing ->
            proc "stack" ["exec", "ghci", "--rts-options="++memoryLimit]
  (ghci, _loads) <- startGhciProcess ghciProc $ \_stream _msg -> return ()
  void $ exec ghci $ ":load " ++ self
  ref <- newMVar ghci
  return $ GhciBackend ref

ghciReload :: GhciBackend -> IO ()
ghciReload (GhciBackend ref) =
  withMVar ref $ \ghci ->
    void $ reload ghci

ghciGenerate :: GhciBackend -> FilePath -> (Int -> IO ()) -> IO ()
ghciGenerate (GhciBackend ref) target cb = withMVar ref $ \ghci ->
  execStream ghci (":main raw --output=" ++ target ++ " --offset=1")
    $ \_ msg ->
      case reads msg of
        [(frameIdx,"")] -> cb frameIdx
        _               -> return ()

memoryLimit :: String
memoryLimit = "-M1G"

-------------------------------------------------------------------------------
-- Websocket API

data WebMessage
  = WebStatus String
  | WebError String
  | WebFrameCount Int
  | WebFrame Int FilePath

sendWebMessage :: Connection -> WebMessage -> IO ()
sendWebMessage conn msg = sendTextData conn $
  case msg of
    WebStatus txt   -> T.pack "status\n" <> T.pack txt
    WebError txt    -> T.pack "error\n" <> T.pack txt
    WebFrameCount n -> T.pack $ "frame_count\n" ++ show n
    WebFrame n path -> T.pack $ "frame\n" ++ show n ++ "\n" ++ path

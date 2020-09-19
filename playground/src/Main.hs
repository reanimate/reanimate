{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications #-}
{-
  Mitigated attacks:
    * unsafeIO: Blocked by fixed import list.
    * TemplateHaskell: Blocked by parsing code.
    * Code injection: Blocked by parsing code.
    * Exhausting memory: Blocked by RTS flags on ghci.
    * Rendering too long: Blocked by both soft and hard timeouts.
    * Generating huge error messages: Error messages are truncated.
    * Take up disk space: Space limits are checked before each frame is rendered.
-}
module Main (main) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Bits
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import           Data.Char                      (toLower)
import           Data.Either                    (isLeft)
import           Data.Hashable
import           Data.IntSet                    (IntSet)
import qualified Data.IntSet as IntSet
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           Data.Time
import           Data.Time.Format
import           GitHash
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SrcLoc   (SrcSpanInfo, noSrcSpan)
import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Ghcid
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.WebSockets
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO.Temp
import           System.IO
import           System.Process
import           System.Timeout
import           Text.Printf

gi :: GitInfo
gi = $$tGitInfoCwd

playgroundCommitDate :: UTCTime
Just playgroundCommitDate = parseTimeM
  True
  defaultTimeLocale
  "%a %b %e %X %Y %z" (giCommitDate gi)

playgroundVersion :: Text
playgroundVersion = T.pack $
  formatTime defaultTimeLocale "%F" playgroundCommitDate ++
  " (" ++ take 5 (giHash gi) ++ ")"

-- Seconds of wall time if the render queue is empty.
totalTimeLimitLong :: NominalDiffTime
totalTimeLimitLong = 30

-- Seconds of wall time if the render queue is full.
totalTimeLimitShort :: NominalDiffTime
totalTimeLimitShort = 5

frameTimeLimit = 5

-- Disk space limit in MiB
diskSpaceLimit :: Double
diskSpaceLimit = 50

-- Limit animation runtimes to 1 minute.
maxAnimationDuration :: Double
maxAnimationDuration = 60

-- Maximum size of error messages
charLimit :: Int
charLimit = 2000

memoryLimit :: String
memoryLimit = "-M1G"

frameRate :: Int
frameRate = 30

main :: IO ()
main = do
  backend <- newBackend
  args <- getArgs
  case args of
    ["test"] -> putStrLn "Test OK"
    ["snippets", folder] -> do
      files <- sort <$> getDirectoryContents folder
      ghci <- takeMVar (backendGhci backend)
      snippets <- mapM (genSnippet ghci)
        [ folder </> file
        | file <- files, takeExtension file == ".hs" ]
      putStr "const snippets = "
      putStrLn $
        "[" ++ intercalate ","
        [ "{" ++
          "\"title\": " ++ show title ++ "," ++
          "\"url\": " ++ show url ++ "," ++
          "\"code\": " ++ show inp ++
          "}\n  "
        | (title, url, inp) <- snippets ] ++
        "];"
      putStrLn $ "const playgroundVersion = " ++ show playgroundVersion ++ ";"
    [] -> do
      root <- cacheDir
      -- The http server is only used for local development.
      tid <- forkIO $ run 10162 (staticApp $ defaultWebAppSettings root)
      serverMain backend `finally` killThread tid
    _ -> do
      hPutStrLn stderr "Invalid arguments"
      exitWith (ExitFailure 1)

genSnippet :: Ghci -> FilePath -> IO (String, String, Text)
genSnippet ghci path = do
    inp <- T.readFile path
    let m = fromParseResult $ parseHaskell inp
        h = sourceHash m
    withHaskellFile m $ \hsFile -> do
      _ <- reqGhcOutput ghci $ ":load " ++ hsFile
      out <- reqGhcOutput ghci "Reanimate.duration animation"
      let dur = read (unlines out) :: Double
          frames = round (dur * fromIntegral frameRate) :: Int
          url = "https://reanimate.clozecards.com/" ++ h ++ "/" ++ show (frames `div` 2) ++ ".svg"
          title = takeWhileEnd (/= '_') (takeBaseName path)
      return (title, url, inp)
  where
    takeWhileEnd f = reverse . takeWhile f . reverse


opts :: ConnectionOptions
opts = defaultConnectionOptions
  { connectionCompressionOptions = PermessageDeflateCompression defaultPermessageDeflate }

serverMain :: Backend -> IO ()
serverMain backend = do
  let options = ServerOptions
        { serverHost = "0.0.0.0"
        , serverPort = 10161
        , serverConnectionOptions = opts
        , serverRequirePong = Nothing }
  runServerWithOptions options $ \pending -> do
    logMsg "New connection received."
    conn <- acceptRequest pending
    requestHandler backend conn

requestHandler :: Backend -> Connection -> IO ()
requestHandler backend conn = loop =<< newMVar True
  where
    loop wantLastRequest = do
      msg <- receiveData conn :: IO T.Text
      _ <- swapMVar wantLastRequest False

      wantThisRequest <- newMVar True
      case parseHaskell msg of
        ParseFailed _ msg -> sendWebMessage conn (WebError msg)
        ParseOk m -> do

          requestRender backend $ Render
            { renderCode       = m
            , renderHash       = sourceHash m
            , renderFrameCount = \i -> sendWebMessage conn (WebFrameCount i)
            , renderFrameReady = \i path -> sendWebMessage conn (WebFrame i path)
            , renderError      = \msg -> sendWebMessage conn (WebError msg)
            , renderWarning    = \msg -> sendWebMessage conn (WebWarning msg)
            , renderWanted     = wantThisRequest
            }
      loop wantThisRequest

data Backend = Backend
  { backendGhci  :: MVar Ghci
  , backendQueue :: MVar Render
  }

data CacheResult
  = CacheMiss
  | CacheHit Int
  | CacheHitPartial Int IntSet
  deriving (Show)

ppCacheResult :: CacheResult -> String
ppCacheResult CacheMiss = "CacheMiss"
ppCacheResult (CacheHit frames) = "CacheHit " ++ show frames
ppCacheResult (CacheHitPartial frames partial) =
  "CacheHitPartial " ++ show (IntSet.size partial) ++ "/" ++ show frames

checkCache :: String -> IO CacheResult
checkCache key = handle (\SomeException{} -> pure CacheMiss) $ do
  root <- cacheDir
  let svgFolder = root </> key
      framesFile = svgFolder </> "frames"
  framesTxt <- readFile framesFile
  frames <- case reads framesTxt of
    [(frames,"")] -> pure frames
    _ -> error "Bad parse"
  files <- listDirectory svgFolder
  let set = IntSet.fromList
        [ idx
        | file <- map dropExtension files
        , (idx,"") <- reads file
        ]
  if IntSet.size set == frames
    then pure $ CacheHit frames
    else pure $ CacheHitPartial frames set

data Render = Render
  { renderCode       :: Module SrcSpanInfo
  , renderHash       :: String
  , renderFrameCount :: Int -> IO ()
  , renderFrameReady :: Int -> FilePath -> IO ()
  , renderError      :: String -> IO ()
  , renderWarning    :: String -> IO ()
  , renderWanted     :: MVar Bool
  }

requestRender :: Backend -> Render -> IO ()
requestRender backend render = do
  cache <- checkCache (renderHash render)
  logMsg $ "Cache: " ++ ppCacheResult cache
  case cache of
    CacheMiss -> void $ forkIO $ putMVar (backendQueue backend) render
    CacheHit frames -> do
      renderFrameCount render frames
      forM_ [0..frames-1] $ \i -> do
        let path = renderHash render </> show i <.> "svg"
        renderFrameReady render i path
    CacheHitPartial frames frameSet -> do
      renderFrameCount render frames
      forM_ (IntSet.toList frameSet) $ \i -> do
        let path = renderHash render </> show i <.> "svg"
        renderFrameReady render i path
      void $ forkIO $ putMVar (backendQueue backend) render{ renderFrameCount = \_ -> return () }

newGhci :: IO Ghci
newGhci = do
  let fastProc = proc "stack" ["exec", "ghci", "--rts-options="++memoryLimit]
  (fastGhci, _loads) <- startGhciProcess fastProc (\_stream msg -> hPutStrLn stderr msg)

  void $ reqGhcOutput fastGhci "import qualified Reanimate"
  void $ reqGhcOutput fastGhci "import qualified Reanimate.Render as Reanimate"
  return fastGhci

newBackend :: IO Backend
newBackend = do
  ghciRef <- newMVar =<< newGhci
  queue <- newEmptyMVar
  root <- cacheDir
  tid <- forkIO $ forever $ do
    req <- takeMVar queue
    let svgFolder = root </> renderHash req
    createDirectoryIfMissing True svgFolder
    startTime <- getCurrentTime
    ghci <- readMVar ghciRef
    let guardTimeout action = do
          now <- getCurrentTime
          emptyQueue <- isEmptyMVar queue
          let timeLimit = if emptyQueue then totalTimeLimitLong else totalTimeLimitShort
          if (diffUTCTime now startTime < timeLimit)
            then action
            else do
              renderWarning req "Render timed out"
              logMsg "Request timed out"
        guardFileSize action = do
          size <- getDirectorySize svgFolder
          if size < round (diskSpaceLimit*1024*1024)
            then action
            else do
              renderWarning req "Disk space limit hit"
              logMsg "Disk space limit hit"
        guardWanted action = do
          wanted <- readMVar (renderWanted req)
          if wanted
            then action
            else logMsg "Results no longer wanted"
        guardGhci cmd action = do
          mbValue <- timeout (round (frameTimeLimit * 1e6)) $
            splitGhciOutput ghci cmd
          case mbValue of
            Nothing -> do
              renderWarning req "Frame render timed out."
              forkIO $ stopGhci ghci
              modifyMVar_ ghciRef (const newGhci)
            Just (err, out)
              | null err -> action out
              | otherwise -> do
                  logMsg $ "Error:\n" ++ take charLimit (unlines err)
                  renderError req (take charLimit (unlines err))
        restartGhci = do
          renderWarning req "Ghci crashed. Restarting."
          logMsg "Ghci crashed. Restarting."
          forkIO $ stopGhci ghci
          modifyMVar_ ghciRef (const newGhci)
        
        loadAndRender hs =
          guardGhci (":load " ++ hs) $ \_ -> guardWanted $
            guardGhci "Reanimate.duration animation" $ \out -> do
              let dur = max 1 (min maxAnimationDuration (read (unlines out))) :: Double
                  frameCount = round (dur * fromIntegral frameRate) :: Int
                  durFile = root </> renderHash req </> "frames"
              writeFile durFile (show frameCount)
              renderFrameCount req frameCount
              renderFrames dur
        renderFrames dur = guardWanted $ guardTimeout $ guardFileSize $ do
          let cmd = printf
                "Reanimate.renderLimitedFrames \"%s\" 0 False %d \
                \(Reanimate.setDuration %f animation)"
                svgFolder frameRate dur
          done <- newIORef False
          mbErrors <- streamGhci ghci cmd $ \msg ->
            case msg of
              "Done" -> writeIORef done True 
              _      -> do
                let frameIdx = read msg
                    path = renderHash req </> show frameIdx <.> "svg"
                renderFrameReady req frameIdx path
          isDone <- readIORef done
          case mbErrors of
            Nothing -> do
              renderWarning req "Frame render timed out."
              forkIO $ stopGhci ghci
              modifyMVar_ ghciRef (const newGhci)
            Just [] | isDone ->
              logMsg "Render finished."
            Just []  ->
              renderFrames dur
            Just errMsgs -> do
              logMsg $ "Error:\n" ++ take charLimit (unlines errMsgs)
              renderError req (take charLimit (unlines errMsgs))
    
    guardWanted $ withHaskellFile (renderCode req) $ \hs -> do
      catch @GhciError
        (loadAndRender hs)
        (\_ -> restartGhci)
  return $ Backend ghciRef queue

getDirectorySize :: FilePath -> IO Integer
getDirectorySize root = do
  files <- getDirectoryContents root
  sum <$> mapM getFileSize [ root </> file | file <- files, takeExtension file == ".svg" ]

cacheDir :: IO FilePath
cacheDir = do
  root <- getXdgDirectory XdgCache "reanimate-playground"
  createDirectoryIfMissing True root
  return root

withHaskellFile :: Module SrcSpanInfo -> (FilePath -> IO a) -> IO a
withHaskellFile m action = withSystemTempFile "playground.hs" $ \target h -> do
    hClose h
    T.writeFile target
      "{-# LANGUAGE OverloadedStrings #-}\n\
      \{-# LANGUAGE FlexibleContexts #-}\n\
      \module Animation where\n\
      \import Reanimate\n\
      \import Reanimate.Builtin.Documentation\n\
      \import Reanimate.Builtin.Images\n\
      \import Reanimate.Builtin.CirclePlot\n\
      \import Reanimate.Builtin.TernaryPlot\n\
      \import Reanimate.Builtin.Slide\n\
      \import Geom2D.CubicBezier.Linear\n\
      \import Reanimate.Transition\n\
      \import Reanimate.Morph.Common\n\
      \import Reanimate.Morph.Linear\n\
      \import Reanimate.Scene\n\
      \import qualified Graphics.SvgTree as SVG\n\
      \import Control.Lens\n\
      \import Control.Monad\n\
      \import qualified Data.Text as T\n\
      \import Linear.V2\n\
      \import Linear.Metric\n\
      \import Linear.Vector\n\
      \import Text.Printf\n\
      \import Codec.Picture.Types\n\
      \import Control.Monad.State\n\
      \-- Used for testing:\n\
      \-- import System.IO.Unsafe\n\
      \-- import Control.Concurrent\n\
      \-- import Control.Exception\n\
      \-- svgDelay d = unsafePerformIO (threadDelay d >> evaluate SVG.None)\n\
      \{-# LINE 1 \"playground\" #-}\n"
    T.appendFile target $ T.pack $ prettyPrint m
    action target

splitGhciOutput :: Ghci -> String -> IO ([String], [String])
splitGhciOutput ghci cmd = do
  out <- newIORef []
  err <- newIORef []
  execStream ghci cmd $ \strm msg ->
    case strm of
      Stdout -> modifyIORef out (++[msg])
      Stderr -> modifyIORef err (++[msg])
  (,) <$> readIORef err <*> readIORef out

reqGhcOutput :: Ghci -> String -> IO [String]
reqGhcOutput ghci cmd = do
  (err, out) <- splitGhciOutput ghci cmd
  unless (null err) $
    error (unlines err)
  return out

streamGhci :: Ghci -> String -> (String -> IO ()) -> IO (Maybe [String])
streamGhci ghci cmd cb = do
  err <- newIORef []
  ret <- timeout tLimit $ execStream ghci cmd $ \strm msg ->
            case strm of
              Stdout -> cb msg
              Stderr -> modifyIORef err (++[msg])
  errMsgs <- readIORef err
  pure  (ret >> pure errMsgs)
 where
   tLimit = round (frameTimeLimit * 1e6)

logMsg :: String -> IO ()
logMsg msg = do
    now <- getCurrentTime
    putStrLn $ formatTime defaultTimeLocale fmt now ++ ": " ++ msg
  where
    fmt = "%F %T%2Q"

encodeInt :: Int -> String
encodeInt i = worker (fromIntegral i) 60
  where
    worker :: Word -> Int -> String
    worker key sh
      | sh < 0 = []
      | otherwise =
        case (key `shiftR` sh) `mod` 64 of
          idx -> alphabet !! fromIntegral idx : worker key (sh-6)
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+$"

sourceHash :: Module SrcSpanInfo -> String
sourceHash = encodeInt . hash . prettyPrint

parseHaskell :: Text -> ParseResult (Module SrcSpanInfo)
parseHaskell txt =
    clean <$> parse (T.unpack txt)
  where
    -- mHead = ModuleHead noSrcSpan (ModuleName noSrcSpan "Animation") Nothing Nothing
    clean (Module _ _ _ _ decls) =
      Module noSrcSpan Nothing [] [] (sort decls)
    clean _ =
      Module noSrcSpan Nothing [] [] []

data WebMessage
  = WebStatus String
  | WebError String
  | WebWarning String
  | WebFrameCount Int
  | WebFrame Int FilePath

sendWebMessage :: Connection -> WebMessage -> IO ()
sendWebMessage conn msg = sendTextData conn $
  case msg of
    WebStatus txt   -> T.pack "status\n" <> T.pack txt
    WebError txt    -> T.pack "error\n" <> T.pack txt
    WebWarning txt  -> T.pack "warning\n" <> T.pack txt
    WebFrameCount n -> T.pack $ "frame_count\n" ++ show n
    WebFrame n path -> T.pack $ "frame\n" ++ show n ++ "\n" ++ path


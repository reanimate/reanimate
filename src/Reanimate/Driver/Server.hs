module Reanimate.Driver.Server
  ( serve
  , findOwnSource ) where

import           Control.Concurrent           (MVar, forkIO, forkOS, killThread,
                                               modifyMVar_, newEmptyMVar,
                                               putMVar, takeMVar)
import           Control.Exception            (SomeException, finally, handle)
import           Control.Monad
import           Control.Monad.Fix            (fix)
import           Data.Maybe
import qualified Data.Text                    as T
import qualified Data.Text.Read               as T
import           Data.Version
import           GHC.Environment              (getFullArgs)
import           Network.WebSockets
import           Paths_reanimate
import           Reanimate.Misc               (runCmdLazy, runCmd_)
import           Reanimate.Monad              (Animation)
import           Reanimate.Render             (render, renderSnippets,
                                               renderSvgs)
import           System.Directory             (doesFileExist, findExecutable,
                                               findFile, listDirectory, withCurrentDirectory)
import           System.Environment           (getArgs, getProgName)
import           System.Exit
import           System.FilePath
import           System.FSNotify
import           System.IO
import           System.IO.Temp
import           Text.ParserCombinators.ReadP
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import           Text.Printf
import           Web.Browser                  (openBrowser)

opts = defaultConnectionOptions
  { connectionCompressionOptions = PermessageDeflateCompression defaultPermessageDeflate }

serve :: Animation -> IO ()
serve animation = do
  watch <- startManager
  hSetBuffering stdin NoBuffering
  self <- findOwnSource
  url <- getDataFileName "viewer/build/index.html"
  putStrLn "Opening browser..."
  bSucc <- openBrowser url
  if bSucc
      then putStrLn "Browser opened."
      else hPutStrLn stderr $ "Failed to open browser. Manually visit: " ++ url
  putStrLn "Listening..."
  runServerWith "127.0.0.1" 9161 opts $ \pending -> do
    putStrLn "New connection received."
    conn <- acceptRequest pending
    slave <- newEmptyMVar
    let handler = modifyMVar_ slave $ \tid -> do

          putStrLn "Reloading code..."
          killThread tid
          tid <- forkOS $ slaveHandler conn self
          return tid
        killSlave = do
          tid <- takeMVar slave
          killThread tid
    stop <- watchFile watch self handler
    putMVar slave =<< forkIO (return ())
    let loop = do
          fps <- receiveData conn :: IO T.Text
          handler
          loop
    loop `finally` (stop >> killSlave)

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

-- FIXME: Move to a different module
-- FIXME: Gracefully disable code reloading if source is missing.
findOwnSource :: IO FilePath
findOwnSource = do
  fullArgs <- getFullArgs
  let stackSource = last fullArgs
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

module Reanimate.Misc
  ( requireExecutable
  , runCmd
  , runCmd_
  , runCmdLazy
  , withTempDir
  , withTempFile
  , renameOrCopyFile
  ) where

import           Control.Concurrent
import           Control.Exception  (catch, evaluate, finally, throw)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Foreign.C.Error
import           GHC.IO.Exception
import           System.Directory   (copyFile, findExecutable, removeFile,
                                     renameFile)
import           System.FilePath    ((<.>))
import           System.IO          (hClose, hGetContents, hIsEOF, hPutStr,
                                     stderr)
import           System.IO.Temp     (withSystemTempDirectory,
                                     withSystemTempFile)
import           System.Process     (readProcessWithExitCode,
                                     runInteractiveProcess, showCommandForUser,
                                     terminateProcess, waitForProcess)


requireExecutable :: String -> IO FilePath
requireExecutable exec = do
  mbPath <- findExecutable exec
  case mbPath of
    Nothing   -> error $ "Couldn't find executable: " ++ exec
    Just path -> return path

runCmd :: FilePath -> [String] -> IO ()
runCmd exec args = do
  ret <- runCmd_ exec args
  case ret of
    Left err -> error $ showCommandForUser exec args ++ ":\n" ++ err
    Right{}  -> return ()

runCmd_ :: FilePath -> [String] -> IO (Either String String)
runCmd_ exec args = do
  (ret, stdout, errMsg) <- readProcessWithExitCode exec args ""
  _                     <- evaluate (length stdout + length errMsg)
  case ret of
    ExitSuccess -> return (Right stdout)
    ExitFailure err | False ->
      return
        $  Left
        $  "Failed to run: "
        ++ showCommandForUser exec args
        ++ "\n"
        ++ "Error code: "
        ++ show err
        ++ "\n"
        ++ "stderr: "
        ++ errMsg
    ExitFailure{} | null errMsg -> -- LaTeX prints errors to stdout. :(
      return $ Left stdout
    ExitFailure{} -> return $ Left errMsg

runCmdLazy
  :: FilePath -> [String] -> (IO (Either String T.Text) -> IO a) -> IO a
runCmdLazy exec args handler = do
  (inp, out, err, pid) <- runInteractiveProcess exec args Nothing Nothing
  hClose inp
  errOutput <- hGetContents err
  _ <- forkIO $ hPutStr stderr errOutput
  let fetch = do
        eof <- hIsEOF out
        if eof
          then do
            _      <- evaluate (length errOutput)
            ret    <- waitForProcess pid
            case ret of
              ExitSuccess   -> return (Left "")
              ExitFailure{} -> return (Left errOutput)
              {-ExitFailure errMsg -> do
                return $ Left $
                  "Failed to run: " ++ showCommandForUser exec args ++ "\n" ++
                  "Error code: " ++ show errMsg ++ "\n" ++
                  "stderr: " ++ stderr-}
          else do
            line <- T.hGetLine out
            return (Right line)
  handler fetch `finally` do
    terminateProcess pid
    _ <- waitForProcess pid
    return ()

-- renameFile fails if we're crossing filesystem boundaries. If this happens,
-- revert back to copyFile + removeFile.
renameOrCopyFile :: FilePath -> FilePath -> IO ()
renameOrCopyFile src dst = renameFile src dst `catch` exdev
 where
  exdev e = if fmap Errno (ioe_errno e) == Just eXDEV
    then copyFile src dst >> removeFile src
    else throw e

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "reanimate"

withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile ext action =
  withSystemTempFile ("reanimate" <.> ext) $ \path hd ->
    hClose hd >> action path

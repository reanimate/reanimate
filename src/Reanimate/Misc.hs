module Reanimate.Misc
  ( requireExecutable
  , runCmd
  , withTempDir
  , withTempFile
  ) where

import           Control.Exception (evaluate, finally, throwIO)
import           System.Directory  (createDirectory, findExecutable,
                                    getTemporaryDirectory,
                                    removeDirectoryRecursive, removeFile)
import           System.Exit       (ExitCode (..))
import           System.FilePath   ((<.>), (</>))
import           System.IO         (hClose, openTempFile)
import           System.Process    (readProcessWithExitCode, showCommandForUser)


requireExecutable :: String -> IO FilePath
requireExecutable exec = do
  mbPath <- findExecutable exec
  case mbPath of
    Nothing   -> error $ "Couldn't find executable: " ++ exec
    Just path -> return path

runCmd :: FilePath -> [String] -> IO ()
runCmd exec args = do
  (ret, stdout, stderr) <- readProcessWithExitCode exec args ""
  evaluate (length stdout + length stderr)
  case ret of
    ExitSuccess -> return ()
    ExitFailure err -> do
      putStrLn $
        "Failed to run: " ++ showCommandForUser exec args ++ "\n" ++
        "Error code: " ++ show err ++ "\n" ++
        "stderr: " ++ show stderr
      throwIO (ExitFailure err)

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
  dir <- getTemporaryDirectory
  (path, handle) <- openTempFile dir "reanimate-XXXXXX"
  hClose handle
  removeFile path
  createDirectory (dir </> path)
  action (dir </> path) `finally` removeDirectoryRecursive (dir </> path)

withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile ext action = do
  dir <- getTemporaryDirectory
  (path, handle) <- openTempFile dir ("reanimate-XXXXXX" <.> ext)
  hClose handle
  action path `finally` removeFile path

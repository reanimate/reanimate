module Reanimate.Misc
  ( requireExecutable
  , runCmd
  , runCmd_
  , runCmdLazy
  , withTempDir
  , withTempFile
  ) where

import           Control.Exception (evaluate, finally)
import qualified Data.Text         as T
import qualified Data.Text.IO      as T
import           System.Directory  (createDirectory, findExecutable,
                                    getTemporaryDirectory, removeFile)
import           System.Exit       (ExitCode (..))
import           System.FilePath   ((<.>), (</>))
import           System.IO         (hClose, hGetContents, hIsEOF, openTempFile)
import           System.Process    (readProcessWithExitCode,
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
    Left err -> error $ "exec " ++ exec ++ show args ++ " failed:\n" ++ err
    Right{} -> return ()

runCmd_ :: FilePath -> [String] -> IO (Either String String)
runCmd_ exec args = do
  (ret, stdout, stderr) <- readProcessWithExitCode exec args ""
  _ <- evaluate (length stdout + length stderr)
  case ret of
    ExitSuccess -> return (Right stdout)
    ExitFailure err -> do
      return $ Left $
        "Failed to run: " ++ showCommandForUser exec args ++ "\n" ++
        "Error code: " ++ show err ++ "\n" ++
        "stderr: " ++ stderr

runCmdLazy :: FilePath -> [String] -> (IO (Either String T.Text) -> IO a) -> IO a
runCmdLazy exec args handler = do
  (inp, out, err, pid) <- runInteractiveProcess exec args Nothing Nothing
  hClose inp
  let fetch = do
        eof <- hIsEOF out
        if eof
          then do
            stderr <- hGetContents err
            _ <- evaluate (length stderr)
            ret <- waitForProcess pid
            case ret of
              ExitSuccess -> return (Left "")
              ExitFailure errMsg -> do
                return $ Left $
                  "Failed to run: " ++ showCommandForUser exec args ++ "\n" ++
                  "Error code: " ++ show errMsg ++ "\n" ++
                  "stderr: " ++ stderr
          else do
            line <- T.hGetLine out
            return (Right line)
  handler fetch `finally` do
    terminateProcess pid
    _ <- waitForProcess pid
    return ()

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
  dir <- getTemporaryDirectory
  (path, handle) <- openTempFile dir "reanimate"
  hClose handle
  removeFile path
  createDirectory (dir </> path)
  action (dir </> path) -- `finally` removeDirectoryRecursive (dir </> path)

withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile ext action = do
  dir <- getTemporaryDirectory
  (path, handle) <- openTempFile dir ("reanimate" <.> ext)
  hClose handle
  action path -- `finally` removeFile path

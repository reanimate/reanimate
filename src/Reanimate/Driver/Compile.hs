module Reanimate.Driver.Compile ( compile ) where

import           Reanimate.Driver.Server (findOwnSource)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
import           System.IO

compile :: [String] -> IO ()
compile opts = do
  mbSelf <- findOwnSource
  case mbSelf of
    Nothing -> do
      hPutStrLn stderr
        "Failed to find source code. Did you already compile the animations?\n\
        \Try running again without the --compile flag."
      exitFailure
    Just self -> do
      let selfDir = takeDirectory self
          selfName = takeBaseName self
          outDir = selfDir </> ".reanimate" </> selfName
          target = outDir </> selfName
          ghcOptions =
              ["-rtsopts", "--make", "-threaded", "-O2"] ++
              ["-odir", outDir, "-hidir", outDir] ++
              [self, "-o", target]
      createDirectoryIfMissing True outDir
      withCurrentDirectory selfDir $ do
        checkExitCode =<< rawSystem "stack" (["ghc", "--"] ++ ghcOptions)
        checkExitCode =<< rawSystem target opts

checkExitCode :: ExitCode -> IO ()
checkExitCode ExitSuccess     = return ()
checkExitCode (ExitFailure n) = exitWith (ExitFailure n)

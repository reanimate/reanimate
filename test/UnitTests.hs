module UnitTests
  ( unitTestFolder
  , compileTestFolder
  , compileVideoFolder
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB8
import           Reanimate.Misc       (withTempDir, withTempFile)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

unitTestFolder :: FilePath -> IO TestTree
unitTestFolder path = do
  files <- getDirectoryContents path
  return $ testGroup "animate"
    [ goldenVsStringDiff file (\ref new -> ["diff", "-a", ref, new]) fullPath (genGolden hsPath)
    | file <- files
    , let fullPath = path </> file
          hsPath = replaceExtension fullPath "hs"
    , takeExtension fullPath == ".golden"
    ]

genGolden :: FilePath -> IO LBS.ByteString
genGolden path = withTempDir $ \tmpDir -> withTempFile ".exe" $ \tmpExecutable -> do
  let ghcOpts = ["-rtsopts", "--make", "-O2"] ++
                ["-odir", tmpDir, "-hidir", tmpDir, "-o", tmpExecutable]
      runOpts = ["+RTS", "-M1G"]
  -- XXX: Check for errors.
  _ <- readProcessWithExitCode "stack" (["ghc","--", path] ++ ghcOpts) ""
  -- ret <- runCmd_ "stack" $ ["ghc", "--"] ++ ghcOptions tmpDir ++ [self, "-o", tmpExecutable]
  -- ["-rtsopts", "--make", "-threaded", "-O2"] ++
  -- ["-odir", tmpDir, "-hidir", tmpDir]
  (inh, outh, errh, _pid) <- runInteractiveProcess tmpExecutable (["test"] ++ runOpts)
    Nothing Nothing
  hSetNewlineMode outh universalNewlineMode
  hClose inh
  hClose errh
  -- str <- hGetContents outh
  -- return $ LB8.pack str
  LBS.hGetContents outh

compileTestFolder :: FilePath -> IO TestTree
compileTestFolder path = do
  files <- getDirectoryContents path
  return $ testGroup "compile"
    [ testCase file $ do
        (ret, _stdout, _stderr) <- readProcessWithExitCode "stack" (["ghc","--", fullPath] ++ ghcOpts) ""
        case ret of
          ExitFailure{} -> assertFailure "Failed to compile"
          ExitSuccess   -> return ()
    | file <- files
    , let fullPath = path </> file
    , takeExtension file == ".hs" || takeExtension file == ".lhs"
    ]
  where
    ghcOpts = ["-fno-code", "-O0"]

compileVideoFolder :: FilePath -> IO TestTree
compileVideoFolder path = do
  files <- getDirectoryContents path
  return $ testGroup "videos"
    [ testCase dir $ do
        (ret, _stdout, _stderr) <- readProcessWithExitCode "stack" (["ghc","--", fullPath] ++ ghcOpts) ""
        case ret of
          ExitFailure{} -> assertFailure "Failed to compile"
          ExitSuccess   -> return ()
    | dir <- files
    , let fullPath = path </> dir </> dir <.> "hs"
    , dir /= "." && dir /= ".."
    ]
  where
    ghcOpts = ["-fno-code", "-O0"]

--------------------------------------------------------------------------------
-- Helpers

-- findAnExecutable :: [String] -> IO (Maybe FilePath)
-- findAnExecutable [] = return Nothing
-- findAnExecutable (x:xs) = do
--   mbExec <- findExecutable x
--   case mbExec of
--     Just exec -> return (Just exec)
--     Nothing   -> findAnExecutable xs
--
-- readFileOptional :: FilePath -> IO String
-- readFileOptional path = do
--   hasFile <- doesFileExist path
--   if hasFile then readFile path else return ""
--
-- assertExitCode :: String -> ExitCode -> Assertion
-- assertExitCode _ ExitSuccess = return ()
-- assertExitCode msg (ExitFailure code) = assertFailure (msg ++ ", code: " ++ show code)
--
-- assertMaybe :: String -> Maybe a -> IO a
-- assertMaybe _ (Just a)  = return a
-- assertMaybe msg Nothing = assertFailure msg

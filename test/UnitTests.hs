module UnitTests (unitTestFolder) where

import           Control.Monad        (forM)
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe           (catMaybes)
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process
import           Test.Tasty
import           Test.Tasty.Golden
import           Reanimate.Misc     (withTempDir,
                                     withTempFile)

unitTestFolder :: FilePath -> IO TestTree
unitTestFolder path = do
  files <- getDirectoryContents path
  return $ testGroup "examples"
    [ goldenVsStringDiff file (\ref new -> ["diff", ref, new]) fullPath (genGolden hsPath)
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
  (inh, outh, errh, pid) <- runInteractiveProcess tmpExecutable (["snippets"] ++ runOpts)
    Nothing Nothing
  hClose inh
  hClose errh
  LBS.hGetContents outh

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

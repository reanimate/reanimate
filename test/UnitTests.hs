module UnitTests
  ( unitTestFolder
  , compileTestFolder
  , compileVideoFolder
  ) where

import           Control.Exception
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.List            (sort)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Unsafe
import           System.Process
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

data BuildSystem
  = Cabal
  | Stack

{-# NOINLINE buildSystem #-}
buildSystem :: BuildSystem
buildSystem = unsafePerformIO $ do
  newbuild <- doesDirectoryExist "dist-newstyle"
  stack <- doesDirectoryExist ".stack-work"
  if newbuild then pure Cabal
    else if stack then pure Stack
    else error "Unknown build system."

{-# NOINLINE unitTestsDisabled #-}
unitTestsDisabled :: Bool
unitTestsDisabled =
  case buildSystem of
    Cabal -> False
    Stack -> unsafePerformIO $ do
      (ret, _, _) <- readProcessWithExitCode "stack" ["exec","--","ghc", "-e", "Reanimate.duration Reanimate.Builtin.Documentation.drawCircle"] ""
      case ret of
        ExitFailure{} -> pure True
        ExitSuccess   -> pure False

unitTestFolder :: FilePath -> IO TestTree
unitTestFolder _ | unitTestsDisabled = return $ testGroup "animate (disabled)" []
unitTestFolder path = do
  files <- sort <$> getDirectoryContents path
  mbWDiff <- findExecutable "wdiff"
  let diff = case mbWDiff of
        Nothing    -> ["diff", "--strip-trailing-cr"]
        Just wdiff -> [wdiff, "--no-common"]
  return $ testGroup "animate"
    [ goldenVsStringDiff file (\ref new -> diff ++ [ref, new]) fullPath (genGolden hsPath)
    | file <- files
    , let fullPath = path </> file
          hsPath = replaceExtension fullPath "hs"
    , takeExtension fullPath == ".golden"
    ]

genGolden :: FilePath -> IO LBS.ByteString
genGolden path = do
  (inh, outh, errh, pid) <- case buildSystem of
    Stack -> runInteractiveProcess "stack" ["runhaskell", path, "test"]
      Nothing Nothing
    Cabal -> runInteractiveProcess "cabal" ["v2-exec", "runhaskell", path, "test"]
      Nothing Nothing
  -- hSetBinaryMode outh True
  -- hSetNewlineMode outh universalNewlineMode
  hClose inh
  out <- BS.hGetContents outh
  err <- T.hGetContents errh
  code <- waitForProcess pid
  case code of
    ExitSuccess   -> return $ LBS.fromChunks [out]
    ExitFailure{} -> error $ "Failed to run: " ++ T.unpack err

compileTestFolder :: FilePath -> IO TestTree
compileTestFolder _ | unitTestsDisabled = return $ testGroup "compile (disabled)" []
compileTestFolder path = do
  files <- sort <$> getDirectoryContents path
  return $ testGroup "compile"
    [ testCase file $ do
        (ret, _stdout, err) <-
          case buildSystem of
            Stack -> readProcessWithExitCode "stack" (["ghc","--", fullPath] ++ ghcOpts) ""
            Cabal -> readProcessWithExitCode "cabal"
              (["v2-exec","ghc","--", "-package", "reanimate", fullPath] ++ ghcOpts) ""
        _ <- evaluate (length err)
        case ret of
          ExitFailure{} -> assertFailure $ "Failed to compile:\n" ++ err
          ExitSuccess   -> return ()
    | file <- files
    , let fullPath = path </> file
    , takeExtension file == ".hs" || takeExtension file == ".lhs"
    , notElem (replaceExtension file "golden") files
    ]
  where
    ghcOpts = ["-fno-code", "-O0", "-Werror", "-Wall"]

compileVideoFolder :: FilePath -> IO TestTree
compileVideoFolder _ | unitTestsDisabled = return $ testGroup "videos (disabled)" []
compileVideoFolder path = do
  exist <- doesDirectoryExist path
  if exist
    then do
      files <- sort <$> getDirectoryContents path
      return $ testGroup "videos"
        [ testCase dir $ do
            (ret, _stdout, err) <-
              case buildSystem of
                Stack  -> readProcessWithExitCode "stack" (["ghc","--", "-i"++path</>dir, fullPath] ++ ghcOpts) ""
                Cabal  -> readProcessWithExitCode "cabal" (["v2-exec", "ghc","--", "-package", "reanimate", "-i"++path</>dir, fullPath] ++ ghcOpts) ""
            _ <- evaluate (length err)
            case ret of
              ExitFailure{} -> assertFailure $ "Failed to compile:\n" ++ err
              ExitSuccess   -> return ()
        | dir <- files
        , let fullPath = path </> dir </> dir <.> "hs"
        , dir /= "." && dir /= ".."
        ]
    else return $ testGroup "videos" []
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

-- withTempDir :: (FilePath -> IO a) -> IO a
-- withTempDir = withSystemTempDirectory "reanimate"

module UnitTests
  ( unitTestFolder
  , compileTestFolder
  , compileVideoFolder
  ) where

import           Control.Exception
import           Control.Monad        (filterM)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.List            (sort)
import           Data.Maybe           (maybe)
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
  | NixBuild NixBuildSystem

data NixBuildSystem
  = NixCabal
  | NixStack

-- required for nix build
nixGHCOptions :: [String]
nixGHCOptions =
  [ "-XPackageImports"
  , "-XPatternSynonyms"
  , "-Wall"
  , "-fno-ignore-asserts"
  , "-DNO_HGEOMETRY"
  , "-hide-package"
  , "matrices"
  ]

findStackAutogen :: IO (Maybe FilePath)
findStackAutogen = do
  let base = ".stack-work" </> "dist"
  baseExists <- doesDirectoryExist base
  if not baseExists
    then return Nothing
    else do
      subDirs <- listDirectories base
      case subDirs of
        [] -> return Nothing
        (arch:_) -> do
          let archPath = base </> arch
          cabalDirs <- listDirectories archPath
          case cabalDirs of
            [] -> return Nothing
            (ver:_) -> return $ Just (archPath </> ver </> "build" </> "autogen")

includeBuild :: NixBuildSystem -> IO [String]
includeBuild NixCabal = return ["-idist/build/autogen"]
includeBuild NixStack = do
  mbPath <- findStackAutogen
  case mbPath of
    Nothing -> return []
    Just p  -> return ["-i" ++ p]

listDirectories :: FilePath -> IO [FilePath]
listDirectories path = do
  contents <- listDirectory path
  filterM (\f -> doesDirectoryExist (path </> f)) contents

{-# NOINLINE buildSystem #-}
buildSystem :: BuildSystem
buildSystem = unsafePerformIO $ do
  exeCabal <- findExecutable "cabal"
  exeStack <- findExecutable "stack"

  newbuild <- doesDirectoryExist "dist-newstyle"
  v1build  <- doesDirectoryExist "dist"
  stack    <- doesDirectoryExist ".stack-work"

  return $ case (exeCabal, exeStack) of 
    (Nothing, _) | v1build  -> NixBuild NixCabal
    (_, Nothing) | stack    -> NixBuild NixStack
    (Just _, _)  | newbuild -> Cabal
    (_, Just _)  | stack    -> Stack
    _                       -> error "Unknown build system."

{-# NOINLINE unitTestsDisabled #-}
unitTestsDisabled :: Bool
unitTestsDisabled =
  case buildSystem of
    Cabal      -> False
    NixBuild _ -> False
    Stack      -> unsafePerformIO $ do
      (ret, _, _) <- readProcessWithExitCode "stack" ["exec","--","ghc", "-e", "Reanimate.duration Reanimate.Builtin.Documentation.drawCircle"] ""
      case ret of
        ExitFailure{} -> pure True
        ExitSuccess   -> pure False

unitTestFolder :: FilePath -> IO TestTree
unitTestFolder _ | unitTestsDisabled = return $ testGroup "animate (disabled)" []
unitTestFolder path = do
  let goldenPath = path </> "golden"
  files <- sort <$> getDirectoryContents goldenPath
  mbWDiff <- findExecutable "wdiff"
  let diff = case mbWDiff of
        Nothing    -> ["diff", "--strip-trailing-cr"]
        Just wdiff -> [wdiff, "--no-common"]
  return $ testGroup "animate"
    [ goldenVsStringDiff file (\ref new -> diff ++ [ref, new]) fullPath (genGolden hsPath)
    | file <- files
    , let fullPath = goldenPath </> file
          hsPath = path </> replaceExtension file "hs"
    , takeExtension fullPath == ".golden"
    ]

genGolden :: FilePath -> IO LBS.ByteString
genGolden path = do
  (inh, outh, errh, pid) <- case buildSystem of
    Stack    -> runInteractiveProcess "stack" ["runhaskell", path, "test"]
      Nothing Nothing
    Cabal    -> runInteractiveProcess "cabal" ["v2-exec", "runhaskell", path, "test"]
      Nothing Nothing
    NixBuild nixbuild -> do
      nixIncludeDir <- includeBuild nixbuild
      runInteractiveProcess "ghc" 
        ( ["-isrc", "-iunix"] 
          ++ nixIncludeDir
          ++ nixGHCOptions 
          ++ ["--run", path, "--", "test"]
        ) Nothing Nothing
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
  goldenFiles <- getDirectoryContents $ path </> "golden"
  return $ testGroup "compile"
    [ testCase file $ do
        (ret, _stdout, err) <-
          case buildSystem of
            Stack    -> readProcessWithExitCode "stack" (["ghc","--", fullPath] ++ ghcOpts) ""
            Cabal    -> readProcessWithExitCode "cabal"
              (["v2-exec","ghc","--", "-package", "reanimate", fullPath] ++ ghcOpts) ""
            NixBuild nixbuild -> do 
              nixIncludeDir <- includeBuild nixbuild
              readProcessWithExitCode "ghc" 
                ( ["-isrc", "-iunix"] 
                  ++ nixIncludeDir
                  ++ nixGHCOptions 
                  ++ [fullPath] ++ (init ghcOpts)
                ) ""
        _ <- evaluate (length err)
        case ret of
          ExitFailure{} -> assertFailure $ "Failed to compile:\n" ++ err
          ExitSuccess   -> return ()
    | file <- files
    , let fullPath = path </> file
    , takeExtension file == ".hs" || takeExtension file == ".lhs"
    , notElem (replaceExtension file "golden") goldenFiles
    ]
  where
    ghcOpts = ["-fno-code", "-O0", "-Wall", "-Werror"]

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
                Stack    -> readProcessWithExitCode "stack" (["ghc","--", "-i"++path</>dir, fullPath] ++ ghcOpts) ""
                Cabal    -> readProcessWithExitCode "cabal" (["v2-exec", "ghc","--", "-package", "reanimate", "-i"++path</>dir, fullPath] ++ ghcOpts) ""
                NixBuild nixbuild -> do
                  nixIncludeDir <- includeBuild nixbuild
                  readProcessWithExitCode "ghc" 
                    ( ["-isrc", "-iunix", "-i"++path</>dir] 
                      ++ nixIncludeDir
                      ++ nixGHCOptions 
                      ++ [fullPath] ++ ghcOpts
                    ) ""
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

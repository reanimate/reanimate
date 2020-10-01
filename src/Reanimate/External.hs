module Reanimate.External
  ( URL,
    SHA256,
    zipArchive,
    tarball,
  )
where

import Control.Monad (unless)
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as B8 (unpack)
import Reanimate.Misc (getReanimateCacheDirectory, withTempFile)
import System.Directory (doesDirectoryExist, findExecutable)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (callProcess)

type URL = String

type SHA256 = String

fetchStaticFile :: URL -> SHA256 -> (FilePath -> FilePath -> IO ()) -> IO FilePath
fetchStaticFile url sha256 unpack = do
  root <- getReanimateCacheDirectory
  let folder = root </> sha256
  hit <- doesDirectoryExist folder
  unless hit $
    downloadFile url $ \path -> do
      inp <- B.readFile path
      let inpSha = B8.unpack (encode (hash inp))
      if inpSha == sha256
        then do
          unpack folder path
        else
          error $
            "URL " ++ url ++ "\n"
              ++ "  Expected SHA256: "
              ++ sha256
              ++ "\n"
              ++ "  Actual SHA256:   "
              ++ inpSha
  return folder

{-# NOINLINE zipArchive #-}

-- | Download and unpack zip archive. The returned path is the unpacked folder.
zipArchive :: URL -> SHA256 -> FilePath
zipArchive url sha256 = unsafePerformIO $
  fetchStaticFile url sha256 $ \folder zipfile ->
    callProcess "unzip" ["-qq", "-d", folder, zipfile]

{-# NOINLINE tarball #-}

-- | Download and unpack tarball. The returned path is the unpacked folder.
tarball :: URL -> SHA256 -> FilePath
tarball url sha256 = unsafePerformIO $
  fetchStaticFile url sha256 $ \folder tarfile ->
    callProcess "tar" ["--overwrite", "--one-top-level=" ++ folder, "-xf", tarfile]

downloadFile :: URL -> (FilePath -> IO a) -> IO a
downloadFile url action = do
  mbCurl <- findExecutable "curl"
  mbWget <- findExecutable "wget"
  case (mbCurl, mbWget) of
    (Just curl, _) -> downloadFileCurl curl url action
    (_, Just wget) -> downloadFileWget wget url action
    (Nothing, Nothing) -> error "curl/wget required to download files"

downloadFileCurl :: FilePath -> URL -> (FilePath -> IO a) -> IO a
downloadFileCurl curl url action = withTempFile "dl" $ \path -> do
  callProcess
    curl
    [ url,
      "--output",
      path,
      "--silent",
      "--show-error",
      "--max-filesize",
      "10M"
    ]
  action path

downloadFileWget :: FilePath -> URL -> (FilePath -> IO a) -> IO a
downloadFileWget wget url action = withTempFile "dl" $ \path -> do
  callProcess
    wget
    [ url,
      "--output-document=" ++ path,
      "--quiet"
    ]
  action path

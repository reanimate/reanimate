module Reanimate.External
  ( URL,
    SHA256,
    zipArchive,
    tarball,

    -- * External Icon Datasets
    simpleIcon,
    simpleIconColor,
    simpleIcons,
    svgLogo,
    svgLogos,
  )
where

import Codec.Picture (PixelRGB8 (..))
import Control.Monad (unless)
import Crypto.Hash.SHA256 (hash)
import Data.Aeson (decodeFileStrict)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as B8 (unpack)
import Data.Char (isSpace, toLower)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Numeric (readHex)
import Reanimate.Animation (SVG)
import Reanimate.Constants (screenHeight, screenWidth)
import Reanimate.Misc (getReanimateCacheDirectory, withTempFile)
import Reanimate.Raster (mkImage)
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable, getDirectoryContents)
import System.FilePath (splitExtension, (<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (callProcess)

-- | Resource address
type URL = String

-- | Resource hash
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
    callProcess "tar" ["--overwrite", "--one-top-level=" ++ folder, "-xzf", tarfile]

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
      "--location",
      "--output",
      path,
      "--silent",
      "--show-error",
      "--max-filesize",
      "10M",
      "--max-time",
      "60"
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



-------------------------------------------------------------------------------
-- SimpleIcons

simpleIconsFolder :: FilePath
simpleIconsFolder =
  tarball
    "https://github.com/simple-icons/simple-icons/archive/3.11.0.tar.gz"
    "NXa8TrHHuQofrPbqTf0pBGt1GDRfuQ4IcQ7kNEk9OcQ="
    </> "simple-icons-3.11.0"

{-# NOINLINE simpleIconPath #-}
simpleIconPath :: String -> FilePath
simpleIconPath key = unsafePerformIO $ do
  let path = simpleIconsFolder </> "icons" </> key <.> "svg"
  hit <- doesFileExist path
  if hit
    then pure path
    else error $ "Key not found in simple-icons dataset: " ++ show key

-- | Icons from <https://simpleicons.org/>. Version 3.11.0. License: CC0
--
-- @
-- let icon = "cplusplus" in `Reanimate.mkGroup`
-- [ `Reanimate.mkBackgroundPixel` (`Codec.Picture.Types.promotePixel` $ `simpleIconColor` icon)
-- , `Reanimate.withFillOpacity` 1 $ `simpleIcon` icon ]
-- @
--
--   <<docs/gifs/doc_simpleIcon.gif>>
simpleIcon :: String -> SVG
simpleIcon = mkImage screenWidth screenHeight . simpleIconPath

-- | Simple Icons svgs do not contain color. Instead, each icon has an associated color value.
simpleIconColor :: String -> PixelRGB8
simpleIconColor key =
  case M.lookup key simpleIconColors of
    Nothing -> error $ "Key not found in simple-icons dataset: " ++ show key
    Just pixel -> pixel

-- | Complete list of all Simple Icons.
simpleIconColors :: Map String PixelRGB8
simpleIconColors = unsafePerformIO $ do
  let path = simpleIconsFolder </> "_data" </> "simple-icons.json"
  mbRet <- decodeFileStrict path
  let parsed = do
        m <- mbRet
        icons <- M.lookup "icons" m
        pure $
          M.fromList
            [ (fromTitle title, parseHex hex) | icon <- icons, Just title <- [M.lookup "title" icon], Just hex <- [M.lookup "hex" icon]
            ]
  case parsed of
    Nothing -> error "Invalid json in simpleIcons"
    Just v -> pure v
  where
    fromTitle :: String -> String
    fromTitle = replaceChars . map toLower

    replaceChars :: String -> String
    replaceChars ('.' : x : xs) = "dot-" ++ replaceChars (x : xs)
    replaceChars "." = "dot"
    replaceChars (x : '.' : []) = replaceChars (x : "-dot")
    replaceChars (x : '.' : xs) = replaceChars (x : "-dot-" ++ xs)
    replaceChars (x : xs)
      | isSpace x || x `elem` "!:'’" = replaceChars xs
    replaceChars ('&' : xs) = "-and-" ++ replaceChars xs
    replaceChars ('+' : xs) = "plus" ++ replaceChars xs
    replaceChars (x : xs)
      | x `elem` "àáâãä" = 'a' : replaceChars xs
      | x `elem` "ìíîï" = 'i' : replaceChars xs
      | x `elem` "èéêë" = 'e' : replaceChars xs
      | x `elem` "šś" = 's' : replaceChars xs
    replaceChars (x : xs) = x : replaceChars xs
    replaceChars [] = []
    parseHex :: String -> PixelRGB8
    parseHex hex = PixelRGB8 (p 0) (p 2) (p 4)
      where
        p offset = case readHex (take 2 $ drop offset hex) of
          [(num, "")] -> num
          _ -> error $ "Invalid hex: " ++ (take 2 $ drop offset hex)

{-# NOINLINE simpleIcons #-}
simpleIcons :: [String]
simpleIcons = unsafePerformIO $ do
  let folder = simpleIconsFolder </> "icons"
  files <- getDirectoryContents folder
  return $
    sort
      [key | file <- files, let (key, ext) = splitExtension file, ext == ".svg"]


svgLogosFolder :: FilePath
svgLogosFolder = tarball
    "https://github.com/gilbarbara/logos/archive/2018.01.tar.gz"
    "kRRA0cF6sVOyqtfVW8EMew4OB4WJcY81DEGS3FLEY8Y="
    </> "logos-2018.01" </> "logos"

{-# NOINLINE svgLogoPath #-}
svgLogoPath :: String -> FilePath
svgLogoPath key = unsafePerformIO $ do
  let path = svgLogosFolder </> key <.> "svg"
  hit <- doesFileExist path
  if hit
    then pure path
    else error $ "Key not found in svg logos dataset: " ++ show key

-- | Icons from <https://svgporn.com/>. Version 3.11.0. License: CC0
--
-- @
-- `svgLogo` "cassandra"
-- @
--
--   <<docs/gifs/doc_svgLogo.gif>>
svgLogo :: String -> SVG
svgLogo = mkImage screenWidth screenHeight . svgLogoPath

{-# NOINLINE svgLogos #-}
-- | Complete list of all SVG Icons.
svgLogos :: [String]
svgLogos = unsafePerformIO $ do
  files <- getDirectoryContents svgLogosFolder
  return $
    sort
      [key | file <- files, let (key, ext) = splitExtension file, ext == ".svg"]
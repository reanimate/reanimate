module Reanimate.Cache
  ( cacheFile -- :: FilePath -> (FilePath -> IO ()) -> IO FilePath
  , cacheMem
  , cacheDisk
  , cacheDiskSvg
  , cacheDiskKey
  , cacheDiskLines
  , encodeInt
  ) where

import           Control.Exception
import           Control.Monad       (unless)
import           Data.Bits
import           Data.Hashable
import           Data.IORef
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Graphics.SvgTree    (Tree, unparse, pattern None)
import           Reanimate.Animation (renderTree)
import           Reanimate.Misc      (renameOrCopyFile)
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Temp
import           System.IO.Unsafe
import           Text.XML.Light      (Content (..), parseXML)

-- Memory cache and disk cache

cacheFile :: FilePath -> (FilePath -> IO ()) -> IO FilePath
cacheFile template gen = do
    root <- getXdgDirectory XdgCache "reanimate"
    createDirectoryIfMissing True root
    let path = root </> template
    hit <- doesFileExist path
    unless hit $ withSystemTempFile template $ \tmp h -> do
      hClose h
      gen tmp
      renameOrCopyFile tmp path
    evaluate path

cacheDisk :: String -> (T.Text -> Maybe a) -> (a -> T.Text) -> (Text -> IO a) -> (Text -> IO a)
cacheDisk cacheType parse render gen key = do
    root <- getXdgDirectory XdgCache "reanimate"
    createDirectoryIfMissing True root
    let path = root </> encodeInt (hash key) <.> cacheType
    hit <- doesFileExist path
    if hit
      then do
        inp <- T.readFile path
        case parse inp of
          Nothing  -> genCache root path
          Just val -> pure val
      else genCache root path
  where
    genCache root path = do
      (tmpPath, tmpHandle) <- openTempFile root (encodeInt (hash key))
      new <- gen key
      T.hPutStr tmpHandle (render new)
      hClose tmpHandle
      renameOrCopyFile tmpPath path
      return new

cacheDiskKey :: Text -> IO Tree -> IO Tree
cacheDiskKey key gen = cacheDiskSvg (const gen) key

cacheDiskSvg :: (Text -> IO Tree) -> (Text -> IO Tree)
cacheDiskSvg = cacheDisk "svg" parse render
  where
    parse txt = case parseXML txt of
      [Elem t] -> Just (unparse t)
      _        -> Nothing
    render = T.pack . renderTree

cacheDiskLines :: (Text -> IO [Text]) -> (Text -> IO [Text])
cacheDiskLines = cacheDisk "txt" parse render
  where
    parse = Just . T.lines
    render = T.unlines


{-# NOINLINE cache #-}
cache :: IORef (Map Text Tree)
cache = unsafePerformIO (newIORef Map.empty)

cacheMem :: (Text -> IO Tree) -> (Text -> IO Tree)
cacheMem gen key = do
  store <- readIORef cache
  case Map.lookup key store of
    Just svg -> return svg
    Nothing -> do
      svg <- gen key
      case svg of
        -- None usually indicates that latex or another tool was misconfigured. In this case,
        -- don't store the result.
        None -> pure svg
        _    -> atomicModifyIORef cache (\m -> (Map.insert key svg m, svg))

encodeInt :: Int -> String
encodeInt i = worker (fromIntegral i) 60
  where
    worker :: Word -> Int -> String
    worker key sh
      | sh < 0 = []
      | otherwise =
        case (key `shiftR` sh) `mod` 64 of
          idx -> alphabet !! fromIntegral idx : worker key (sh-6)
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+$"

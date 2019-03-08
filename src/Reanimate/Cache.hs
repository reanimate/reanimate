module Reanimate.Cache
  ( cacheMem
  , cacheDisk
  , cacheDiskSvg
  , cacheDiskLines
  ) where

import           Control.Exception
import           Data.Hashable
import           Data.IORef
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T
import           Graphics.SvgTree   (Tree (..), parseSvgFile, unparse)
import           Reanimate.Monad    (renderTree)
import           Reanimate.Svg      (unbox)
import Text.XML.Light ( Content(..), parseXML )
import           System.Directory
import           System.FilePath
import           System.IO.Unsafe

-- Memory cache and disk cache

cacheDisk :: (T.Text -> Maybe a) -> (a -> T.Text) -> (Text -> IO a) -> (Text -> IO a)
cacheDisk parse render gen key = do
    root <- getXdgDirectory XdgCache "reanimate"
    createDirectoryIfMissing True root
    let path = root </> show (hash key)
    hit <- doesFileExist path
    if hit
      then do
        inp <- T.readFile path
        case parse inp of
          Nothing -> do
            let tmp = path <.> "tmp"
            new <- gen key
            T.writeFile tmp (render new)
            renameFile tmp path
            return new
          Just val -> pure val
      else do
        let tmp = path <.> "tmp"
        new <- gen key
        T.writeFile tmp (render new)
        renameFile tmp path
        return new

cacheDiskSvg :: (Text -> IO Tree) -> (Text -> IO Tree)
cacheDiskSvg = cacheDisk parse render
  where
    parse txt = case parseXML txt of
      [Elem t] -> Just (unparse t)
      _   -> Nothing
    render = T.pack . renderTree

cacheDiskLines :: (Text -> IO [Text]) -> (Text -> IO [Text])
cacheDiskLines = cacheDisk parse render
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
        None -> pure None
        _ -> atomicModifyIORef cache (\store -> (Map.insert key svg store, svg))

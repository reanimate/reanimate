module Cache (lookupCache, insertCache) where

import Control.Concurrent
import Control.Exception
import qualified Data.IntMap as M
import System.Directory
import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Hashable

valueFilePath :: Text -> IO FilePath
valueFilePath key = do
  tmp <- getTemporaryDirectory
  return $ tmp </> "reanimate" ++ show (hash key) <.> "svgs"


insertCache :: Text -> [Text] -> IO ()
insertCache key value = do
  cacheFile <- valueFilePath key
  T.writeFile cacheFile (T.unlines value)

lookupCache :: Text -> IO (Maybe [Text])
lookupCache key = do
    cacheFile <- valueFilePath key
    do svgs <- T.lines <$> T.readFile cacheFile
       evaluate (svgs)
       return (Just svgs)
     `catch` \SomeException{} -> return Nothing

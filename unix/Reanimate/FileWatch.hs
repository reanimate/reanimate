module Reanimate.FileWatch
  ( watchFile ) where

import           System.INotify

watchFile :: FilePath -> IO () -> IO ()
watchFile path handler = do
  notify <- initINotify
  addWatch notify [Modify] path (const handler)
  return ()

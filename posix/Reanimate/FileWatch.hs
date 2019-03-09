module Reanimate.FileWatch
  ( watchFile ) where

import Control.Concurrent
import           System.Directory

watchFile :: FilePath -> IO () -> IO ()
watchFile path handler = do
    t0 <- getModificationTime path
    forkIO $ loop t0
    return ()
  where
    loop lastModTime = do
      t1 <- getModificationTime path
      if t1 /= lastModTime
        then handler >> loop t1
        else threadDelay (10^3 * delay) >> loop lastModTime
    delay = 1000 -- pool delay in ms.

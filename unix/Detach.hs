module Detach (detach) where

import           Control.Concurrent
import           Control.Monad
import           System.Posix.IO
import           System.Posix.Process (createSession, forkProcess)

detach :: IO () -> IO Bool
detach daemon = do
  void $ forkProcess $ do
    devnull <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
    void $ dupTo devnull stdInput
    void $ dupTo devnull stdOutput
    void $ dupTo devnull stdError
    closeFd devnull
    void createSession
    void $ forkProcess daemon
  threadDelay (10^(6::Int))
  return True

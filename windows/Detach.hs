module Detach (detach) where

import Control.Monad
import Control.Concurrent

detach :: IO () -> IO Bool
detach action = do
  void $ forkIO action
  threadDelay (10^(6::Int))
  return False

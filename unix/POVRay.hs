module POVRay
  ( povrayApp
  , runPOVRay
  ) where

import Reanimate.Misc (requireExecutable, runCmd)

-- | Name of the POV-Ray executable
povrayApp :: String
povrayApp = "povray"

-- | Run the POV-Ray executable with arguments
runPOVRay :: [String] -> IO ()
runPOVRay args = do
  exec <- requireExecutable povrayApp
  runCmd exec args

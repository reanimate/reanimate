module POVRay
  ( povrayApp
  , runPOVRay
  ) where

import           Reanimate.Misc       (requireExecutable, runCmd)
import           System.IO            (hClose, hPutStrLn)
import           System.IO.Temp       (withSystemTempFile)

-- | Name of the POV-Ray executable
povrayApp :: String
povrayApp = "pvengine64"  -- Assumes 64 bit Windows

-- | Run the POV-Ray executable with arguments
runPOVRay :: [String] -> IO ()
runPOVRay args = do
  exec <- requireExecutable povrayApp
  let exec' = '"' : exec ++ "\""  -- Wrap exec in "" because the path is likely
                                  -- to includes spaces
      args' = "/EXIT" : args  -- Note [/EXIT special command-line option]
      command = concatMap (' ':) (exec' : args')
  -- Note [Use of a batch file]
  withSystemTempFile "pvcommand.bat" $ \path h -> do
    hPutStrLn h command
    hClose h
    runCmd path []

{-
Note [/EXIT special command-line option]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"The /EXIT command tells POV-Ray to perform the render given by the other
command-line options, combined with previously-set options such as internal
command-line settings, INI file, source file, and so forth, and then to exit. By
default, if this switch is not present, POV-Ray for Windows will remain running
after the render is complete. This switch only applies to renders started by
other options on the command-line. It will not affect renders started manually
from within POV-Ray for Windows itself."
source: https://www.povray.org/documentation/view/3.6.1/603/

Note [Use of batch file]
~~~~~~~~~~~~~~~~~~~~~~~~

POV-Ray for Windows assumes that anything on the command-line that is not a
switch is the name of an INI file. Switches are preceded by a plus or minus.
However, System.Process.createProcess_ wraps all arguments in "". POV_Ray for
Windows appears to interpret a switch wrapped in "" as the name of an INI file.

The work around used here is to write the POV-Ray command line to a batch file.
-}

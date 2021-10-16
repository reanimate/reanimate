module POVRay
  ( povrayApp
  , mkPovrayImage'
  ) where

import           Data.Hashable        (Hashable (hash))
import           Data.Text            (Text)
import qualified Data.Text            as T (concat, pack)
import qualified Data.Text.IO         as T (writeFile)
import           Reanimate.Cache      (cacheFile, encodeInt)
import           Reanimate.Misc       (requireExecutable, runCmd)
import           Reanimate.Parameters (pNoExternals)
import           System.FilePath      (replaceExtension, (<.>))
import           System.IO            (hClose, hPutStrLn)
import           System.IO.Temp       (withSystemTempFile)

povrayApp :: String
povrayApp = "pvengine64"  -- Assumes 64 bit Windows

mkPovrayImage' :: [String] -> Text -> IO FilePath
mkPovrayImage' _ _ | pNoExternals = pure "/povray/has/been/disabled"
mkPovrayImage' args script = cacheFile template $ \target -> do
  exec <- requireExecutable povrayApp
  let pov_file = replaceExtension target "pov"
      exec' = '"' : exec ++ "\""  -- Wrap exec in "" because the path is likely
                                  -- to includes spaces
      otherArgs = [ "-D"
                  , "+UA"
                  , "+I" ++ pov_file
                  , "+O" ++ target
                  , "/EXIT"  -- Note [/EXIT special command-line option]
                  ]
      command = concatMap (' ':) (exec' : args ++ otherArgs)
  T.writeFile pov_file script
  -- Note [Use of a batch file]
  withSystemTempFile "pvcommand.bat" $ \path h -> do
    hPutStrLn h command
    hClose h
    runCmd path []
 where
  template = encodeInt (hash key) <.> "png"
  key = T.concat (script:map T.pack args)

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

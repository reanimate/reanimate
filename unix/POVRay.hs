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

povrayApp :: String
povrayApp = "povray"

mkPovrayImage' :: [String] -> Text -> IO FilePath
mkPovrayImage' _ _ | pNoExternals = pure "/povray/has/been/disabled"
mkPovrayImage' args script = cacheFile template $ \target -> do
  exec <- requireExecutable povrayApp
  let pov_file = replaceExtension target "pov"
  T.writeFile pov_file script
  runCmd exec (args ++ ["-D", "+UA", "+I" ++ pov_file, "+o" ++ target])
 where
  template = encodeInt (hash key) <.> "png"
  key = T.concat (script:map T.pack args)

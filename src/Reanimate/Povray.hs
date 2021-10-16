{-# LANGUAGE OverloadedStrings #-}
{-|
  [POV-Ray](http://povray.org/) (the Persistance of Vision Raytracer) is a
  scriptable raytracer. All POV-Ray functions are cached and will reuse images
  when scripts stay the same.

  The functions come in two versions, for example 'povray' and 'povray''. The
  versions without an apostrophe character yield a 'Tree'. The versions with a
  final apostrophe character yield a 'FilePath' of a PNG file containing the
  resulting image.

  The functions differ in their use of the POV-Ray @+W@ (width), @+H@ (height)
  and @+A@ or @-A@ (anti-aliasing) switches, as set out below. Resolutions are
  \'width x height\'.

  Example of use:

  > povray args script

  where @args@ is a list of other POV-Ray command-line switches (if any), and
  @script@ is a POV-Ray scene description specified in the POV-Ray /scene/
  /description language/.
-}
module Reanimate.Povray
  ( povray
  , povrayQuick
  , povraySlow
  , povrayExtreme
  , povray'
  , povrayQuick'
  , povraySlow'
  , povrayExtreme'
  ) where

import           Data.Hashable              (Hashable (hash))
import           Data.Text                  (Text)
import qualified Data.Text                  as T (concat, pack)
import qualified Data.Text.IO               as T (writeFile)
import           Graphics.SvgTree           (Tree)
import           POVRay                     (runPOVRay)
import           Reanimate.Cache            (cacheFile, encodeInt)
import           Reanimate.Constants        (screenHeight, screenWidth)
import           Reanimate.Parameters       (pNoExternals)
import           Reanimate.Raster           (mkImage)
import           Reanimate.Svg.Constructors (mkText)
import           System.FilePath            (replaceExtension, (<.>))
import           System.IO.Unsafe           (unsafePerformIO)



povrayRaw :: [String] -> Text -> Tree
povrayRaw args script =
  unsafePerformIO $ mkPovrayImage args script

povrayRaw' :: [String] -> Text -> FilePath
povrayRaw' args script =
  unsafePerformIO $ mkPovrayImage' args script

-- | Run the POV-Ray raytracer with a default resolution of 320x180
--   and antialiasing enabled. The resulting image is scaled to fit
--   the screen exactly.
povray :: [String] -> Text -> Tree
povray args = povrayRaw (["+H180", "+W320", "+A"] ++ args)

-- | Run the POV-Ray raytracer with a default resolution of 320x180
--   and antialiasing enabled. The FilePath points to a PNG file
--   containing the resulting image.
povray' :: [String] -> Text -> FilePath
povray' args = povrayRaw' (["+H180", "+W320", "+A"] ++ args)

-- | Run the POV-Ray raytracer with a default resolution of 320x180
--   but without antialiasing. The resulting image is scaled to fit
--   the screen exactly.
povrayQuick :: [String] -> Text -> Tree
povrayQuick args = povrayRaw (["+H180", "+W320"] ++ args)

-- | Run the POV-Ray raytracer with a default resolution of 320x180
--   but without antialiasing. The FilePath points to a PNG file
--   containing the resulting image.
povrayQuick' :: [String] -> Text -> FilePath
povrayQuick' args = povrayRaw' (["+H180", "+W320"] ++ args)

-- | Run the POV-Ray raytracer with a default resolution of 2560x1440
--   and antialiasing enabled. The resulting image is scaled to fit
--   the screen exactly.
povraySlow :: [String] -> Text -> Tree
povraySlow args = povrayRaw (["+H1440", "+W2560", "+A"] ++ args)

-- | Run the POV-Ray raytracer with a default resolution of 2560x1440
--   and antialiasing enabled. The FilePath points to a PNG file
--   containing the resulting image.
povraySlow' :: [String] -> Text -> FilePath
povraySlow' args = povrayRaw' (["+H1440", "+W2560", "+A"] ++ args)

-- | Run the POV-Ray raytracer with a default resolution of 3840x2160
--   and antialiasing enabled. The resulting image is scaled to fit
--   the screen exactly.
povrayExtreme :: [String] -> Text -> Tree
povrayExtreme args = povrayRaw (["+H2160", "+W3840", "+A"] ++ args)

-- | Run the POV-Ray raytracer with a default resolution of 3840x2160
--   and antialiasing enabled. The FilePath points to a PNG file
--   containing the resulting image.
povrayExtreme' :: [String] -> Text -> FilePath
povrayExtreme' args = povrayRaw' (["+H2160", "+W3840", "+A"] ++ args)

mkPovrayImage :: [String] -> Text -> IO Tree
mkPovrayImage _ script | pNoExternals = pure $ mkText script
mkPovrayImage args script =
  mkImage screenWidth screenHeight <$> mkPovrayImage' args script

mkPovrayImage' :: [String] -> Text -> IO FilePath
mkPovrayImage' _ _ | pNoExternals = pure "/povray/has/been/disabled"
mkPovrayImage' args script = cacheFile template $ \target -> do
  let pov_file = replaceExtension target "pov"
      otherArgs = ["-D", "+UA", "+I" ++ pov_file, "+O" ++ target]
  T.writeFile pov_file script
  runPOVRay $ args ++ otherArgs
 where
  template = encodeInt (hash key) <.> "png"
  key = T.concat (script:map T.pack args)

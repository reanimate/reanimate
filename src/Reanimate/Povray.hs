{-# LANGUAGE OverloadedStrings #-}
{-|
  [Povray](http://povray.org/) is a scriptable raytracer. All povray functions
  are cached and will reuse images when scripts stay the same.
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

import           Data.Text                  (Text)
import           Graphics.SvgTree           (Tree)
import           POVRay                     (mkPovrayImage')
import           Reanimate.Constants        (screenHeight, screenWidth)
import           Reanimate.Parameters       (pNoExternals)
import           Reanimate.Raster           (mkImage)
import           Reanimate.Svg.Constructors (mkText)
import           System.IO.Unsafe           (unsafePerformIO)

povrayRaw :: [String] -> Text -> Tree
povrayRaw args script =
  unsafePerformIO $ mkPovrayImage args script

povrayRaw' :: [String] -> Text -> FilePath
povrayRaw' args script =
  unsafePerformIO $ mkPovrayImage' args script

-- | Run the povray raytracer with a default resolution of 320x180
--   and antialiasing enabled. The resulting image is scaled to fit
--   the screen exactly.
povray :: [String] -> Text -> Tree
povray args = povrayRaw (["+H180","+W320", "+A"] ++ args)

-- | Run the povray raytracer with a default resolution of 320x180
--   and antialiasing enabled. The FilePath points to a PNG file
--   containing the resulting image.
povray' :: [String] -> Text -> FilePath
povray' args = povrayRaw' (["+H180","+W320", "+A"] ++ args)

-- | Run the povray raytracer with a default resolution of 320x180
--   but without antialiasing. The resulting image is scaled to fit
--   the screen exactly.
povrayQuick :: [String] -> Text -> Tree
povrayQuick args = povrayRaw (["+H180","+W320"] ++ args)

-- | Run the povray raytracer with a default resolution of 320x180
--   but without antialiasing. The FilePath points to a PNG file
--   containing the resulting image.
povrayQuick' :: [String] -> Text -> FilePath
povrayQuick' args = povrayRaw' (["+H180","+W320"] ++ args)

-- | Run the povray raytracer with a default resolution of 1440x2560
--   and antialiasing enabled. The FilePath points to a PNG file
--   containing the resulting image.
povraySlow :: [String] -> Text -> Tree
povraySlow args = povrayRaw (["+H1440","+W2560", "+A"] ++ args)

-- | Run the povray raytracer with a default resolution of 1440x2560
--   and antialiasing enabled. The FilePath points to a PNG file
--   containing the resulting image.
povraySlow' :: [String] -> Text -> FilePath
povraySlow' args = povrayRaw' (["+H1440","+W2560", "+A"] ++ args)

-- | Run the povray raytracer with a default resolution of 2160x3840
--   and antialiasing enabled. The FilePath points to a PNG file
--   containing the resulting image.
povrayExtreme :: [String] -> Text -> Tree
povrayExtreme args = povrayRaw (["+H2160","+W3840", "+A"] ++ args)

-- | Run the povray raytracer with a default resolution of 2160x3840
--   and antialiasing enabled. The FilePath points to a PNG file
--   containing the resulting image.
povrayExtreme' :: [String] -> Text -> FilePath
povrayExtreme' args = povrayRaw' (["+H2160","+W3840", "+A"] ++ args)

mkPovrayImage :: [String] -> Text -> IO Tree
mkPovrayImage _ script | pNoExternals = pure $ mkText script
mkPovrayImage args script =
  mkImage screenWidth screenHeight <$> mkPovrayImage' args script

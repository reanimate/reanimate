{-# LANGUAGE OverloadedStrings #-}
module Reanimate.Blender
  ( blender
  , blender'
  ) where

import           Data.Hashable
import           Data.Text                  (Text)
import qualified Data.Text.IO               as T
import           Graphics.SvgTree           (Tree (..))
import           Reanimate.Animation
import           Reanimate.Cache
import           Reanimate.Constants
import           Reanimate.Misc
import           Reanimate.Parameters
import           Reanimate.Raster
import           Reanimate.Svg.Constructors
import           System.FilePath            (replaceExtension, (<.>))
import           System.IO.Unsafe           (unsafePerformIO)

blender :: Text -> SVG
blender script =
  unsafePerformIO $ mkBlenderImage script

blender' :: Text -> FilePath
blender' script =
  unsafePerformIO $ mkBlenderImage' script

mkBlenderImage :: Text -> IO Tree
mkBlenderImage script | pNoExternals = pure $ mkText script
mkBlenderImage script =
  mkImage screenWidth screenHeight <$> mkBlenderImage' script

mkBlenderImage' :: Text -> IO FilePath
mkBlenderImage' _ | pNoExternals = pure "/blender/has/been/disabled"
mkBlenderImage' script = cacheFile template $ \target -> do
    exec <- requireExecutable "blender"
    let py_file = replaceExtension target "py"
    T.writeFile py_file script
    runCmd exec [ "--background","--render-format", "PNG"
                , "--python-exit-code", "1"
                , "--render-output", target, "--python", py_file]
  where
    template = encodeInt (hash script) <.> "png"

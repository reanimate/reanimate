{-# LANGUAGE OverloadedStrings #-}
module Reanimate.Blender
  ( blender
  , blender'
  ) where

import           Codec.Picture.Png
import qualified Data.ByteString            as B
import           Data.Text                  (Text)
-- import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Graphics.SvgTree           (Tree (..))
import           Reanimate.Cache
import           Reanimate.Misc
import           Reanimate.Raster
import           Reanimate.Parameters
import           Reanimate.Animation
import           Reanimate.Svg.Constructors
import           System.FilePath            (replaceExtension, (<.>))
import           System.IO.Unsafe           (unsafePerformIO)
import           Data.Hashable

blender :: Text -> SVG
blender script =
  (unsafePerformIO $ mkBlenderImage script)

blender' :: Text -> FilePath
blender' script =
  (unsafePerformIO $ mkBlenderImage' script)

mkBlenderImage :: Text -> IO Tree
mkBlenderImage script | pNoExternals = pure $ mkText script
mkBlenderImage script = do
    png <- B.readFile =<< mkBlenderImage' script
    case decodePng png of
      Left{}    -> error "bad image"
      Right img -> return $ center $ scaleToSize 16 9 $ embedDynamicImage img

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
    template = show (hash script) <.> "png"

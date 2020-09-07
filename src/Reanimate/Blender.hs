{-# LANGUAGE OverloadedStrings #-}
{-|
  [Blender](https://www.blender.org/) is a free and open-source 3D graphics toolkit.
  It is usually used through a graphical user-interface but can also be
  scripted via Python. These Blender scripts can access 100% of Blender's
  functionality and offer a convenient way of coding 3D effects.

  Running Blender can be time-consuming but heavy caching means scripts
  are only run when they change.

  Blender cheatsheet:

> # To generate with a transparent background, set film_transparent = True:
> bpy.context.scene.render.film_transparent = True
>
> # Filmic is great for photorealism but bad for animations.
> # If you want your textures to keep their exact color values,
> # set the view_transform to 'Standard':
> bpy.context.scene.view_settings.view_transform = 'Standard'
>
> # Blender's default render engine is 'EEVEE', fast but not a raytracer.
> # To switch to raytracing, set the engine to 'CYCLES':
> bpy.context.scene.render.engine = 'CYCLES'
>
> # Rendering at full resolution can be slow. When developing, try
> # decreasing the resolution_percentage for faster renders.
> bpy.context.scene.render.resolution_percentage = 10
>
> # The resolution of the final image are set by resolution_x and resolution_y:
> bpy.context.scene.render.resolution_x = 320
> bpy.context.scene.render.resolution_y = 180

-}
module Reanimate.Blender
  ( blender
  , blender'
  ) where

import           Data.Hashable
import           Data.Text                  (Text)
import qualified Data.Text.IO               as T
import           Graphics.SvgTree           (Tree)
import           Reanimate.Animation
import           Reanimate.Cache
import           Reanimate.Constants
import           Reanimate.Misc
import           Reanimate.Parameters
import           Reanimate.Raster
import           Reanimate.Svg.Constructors
import           System.FilePath            (replaceExtension, (<.>))
import           System.IO.Unsafe           (unsafePerformIO)

-- | Run a Blender script and embed the resulting image file. The
--   image will be scaled to fit the screen exactly (assuming a default
--   canvas layout). Note that Blender resolution defaults to 1920x1080
--   but can be changed in the script code.
blender :: Text -> SVG
blender script =
  unsafePerformIO $ mkBlenderImage script

-- | Generate Blender image as a separate PNG file. Can be embedded with
--   `mkImage`.
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

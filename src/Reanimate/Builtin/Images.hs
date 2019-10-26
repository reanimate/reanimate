module Reanimate.Builtin.Images
  ( svgLogo
  ) where

import qualified Data.ByteString     as B
import           Graphics.SvgTree    (parseSvgFile)
import           Paths_reanimate
import           Reanimate.Animation
import           Reanimate.Svg
import           System.IO.Unsafe

-- | <<docs/gifs/doc_svgLogo.gif>>
svgLogo :: SVG
svgLogo = unsafePerformIO $ do
  svg_file <- getDataFileName "data/svg-logo.svg"
  svg_data <- B.readFile svg_file
  case parseSvgFile svg_file svg_data of
    Nothing  -> error "Malformed svg"
    Just svg -> return $ embedDocument svg

module Reanimate.Builtin.Images
  ( svgLogo
  , haskellLogo
  , githubIcon
  ) where

import qualified Data.ByteString     as B
import           Graphics.SvgTree    (parseSvgFile)
import           Paths_reanimate
import           Reanimate.Animation
import           Reanimate.Svg
import           System.IO.Unsafe

embedImage :: FilePath -> IO SVG
embedImage key = do
  svg_file <- getDataFileName key
  svg_data <- B.readFile svg_file
  case parseSvgFile svg_file svg_data of
    Nothing  -> error "Malformed svg"
    Just svg -> return $ embedDocument svg

-- | <<docs/gifs/doc_svgLogo.gif>>
svgLogo :: SVG
svgLogo = unsafePerformIO $ embedImage "data/svg-logo.svg"

-- | <<docs/gifs/doc_haskellLogo.gif>>
haskellLogo :: SVG
haskellLogo = unsafePerformIO $ embedImage "data/haskell.svg"

-- | <<docs/gifs/doc_githubIcon.gif>>
githubIcon :: SVG
githubIcon = unsafePerformIO $ embedImage "data/github-icon.svg"

{-|
Module      : Reanimate.Builtin.Images
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX

Collection of built-in images.

-}
module Reanimate.Builtin.Images
  ( svgLogo
  , haskellLogo
  , githubIcon
  , githubWhiteIcon
  , smallEarth
  ) where

import           Codec.Picture
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

loadJPG :: FilePath -> Image PixelRGBA8
loadJPG key = unsafePerformIO $ do
  jpg_file <- getDataFileName key
  dat <- B.readFile jpg_file
  case decodeJpeg dat of
    Left err  -> error err
    Right img -> return $ convertRGBA8 img

{- HLINT ignore svgLogo -}
-- | <<docs/gifs/doc_svgLogo.gif>>
svgLogo :: SVG
svgLogo = unsafePerformIO $ embedImage "data/svg-logo.svg"

{- HLINT ignore haskellLogo -}
-- | <<docs/gifs/doc_haskellLogo.gif>>
haskellLogo :: SVG
haskellLogo = unsafePerformIO $ embedImage "data/haskell.svg"

{- HLINT ignore githubIcon -}
-- | <<docs/gifs/doc_githubIcon.gif>>
githubIcon :: SVG
githubIcon = unsafePerformIO $ embedImage "data/github-icon.svg"

{-# NOINLINE githubWhiteIcon #-}
-- | <<docs/gifs/doc_githubWhiteIcon.gif>>
githubWhiteIcon :: SVG
githubWhiteIcon = unsafePerformIO $ embedImage "data/github-icon-white.svg"

-- | 300x150 equirectangular earth
--
--   <<docs/gifs/doc_smallEarth.gif>>
smallEarth :: Image PixelRGBA8
smallEarth = loadJPG "data/small_earth.jpg"

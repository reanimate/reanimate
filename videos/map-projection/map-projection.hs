#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import           Codec.Picture
import           Codec.Picture.Jpg
import qualified Data.ByteString   as BS
import           Reanimate
import           System.IO.Unsafe

main :: IO ()
main = reanimate $ sceneAnimation $do
    play $ animate $ const $
      scaleToSize screenWidth screenHeight $
      embedImage equirectangular
    play $ animate $ const $
      scaleToSize screenWidth screenHeight $
      embedImage mercator

equirectangular :: Image PixelRGB8
equirectangular = unsafePerformIO $ do
  dat <- BS.readFile "earth.jpg"
  case decodeJpeg dat of
    Left err  -> error err
    Right img -> return $ convertRGB8 img

mercator :: Image PixelRGB8
mercator =
    generateImage fn w h
  where
    w = imageWidth equirectangular
    h = imageHeight equirectangular
    fn xPx yPx =
      let -- x = fromToS (-pi) pi (fromIntegral xPx / fromIntegral (w-1))
          y = fromToS (-pi/2) (pi/2) (fromIntegral yPx / fromIntegral (h-1))
          orig_y = round (fromToS 0 (fromIntegral h) ((atan (sinh y) + (pi/2)) / pi))
      in pixelAt equirectangular xPx orig_y

-- pixelAt
-- generateImage

{-
image X: -pi to +pi
image Y: -pi/2 to +pi/2

x=long
y=lat

Web Mercator
x = long
y = -log(tan(pi/4 + lat/2))

x = x_rect
y = (-log(tan(pi/4 + y_rect/2)))

y_rect = atan (sinh y)

-}

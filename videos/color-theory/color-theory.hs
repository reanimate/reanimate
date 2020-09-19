#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import qualified Data.Text                     as T

import           Codec.Picture
import           Codec.Picture.Types
import           Data.Word
import           Graphics.SvgTree                  hiding ( Image
                                                          , imageHeight
                                                          , imageWidth
                                                          )
import           Numeric
import           Reanimate
import           Reanimate.ColorComponents
import           Reanimate.Builtin.Slide
import           Reanimate.Raster
import           Reanimate.Scene
import           Reanimate.Transition
import           System.IO.Unsafe

import           Grid
import           Spectrum
import           EndScene
import           Transcript
import           Common

-- beginWord = findWord ["why"] "theory"
-- beginWord = findWord ["theory"] "spectrum"
-- beginWord = findWord ["simplify"] "awkward"
-- beginWord = findWord ["xyz"] "squeezed"
-- beginWord = findWord ["xyz"] "Taking"
-- beginWord = findWord ["rgb"] "Finally"
-- beginWord = findWord ["hsv"] "we"
-- beginWord = findWord ["lab"] "The"
-- beginWord = findWord ["pyramid"] "Taking"
-- beginWord = findWord ["made"] "instead"
beginWord = findWord ["colormap"] "Shades"

-- endWord = findWord ["xyz"] "colors"
-- endWord = findWord ["simplify"] "All"
-- endWord = findWord ["xyz"] "sRGB"
-- endWord = findWord ["rgb"] "While"
-- endWord = findWord ["hsv"] "saturation"
-- endWord = findWord ["lab"] "brightness"
endWord = findWord ["colormap"] "1970s"

main :: IO ()
main =
  reanimate
    -- $ dropA (wordStart beginWord)
    -- $ takeA (wordEnd endWord)
    -- $ takeA 20
    $ scene
    $ do
        newSpriteSVG_ $ mkBackground "black"
        monalisaScene
        transitionO transition1 1 falseColorScene $
          transitionO transition2 1 scene2 $
            transitionO transition3 2 gridScene endScene
 where
  transition1 = signalT (curveS 2) slideDownT
  transition2 = signalT (curveS 2) slideUpT
  transition3 = signalT (curveS 2) slideDownT

monalisaScene :: Scene s ()
monalisaScene = spriteScope $ do
  -- Fade in numbers
  waitUntil $ wordStart $ findWord [] "wall"
  hex <- newSpriteSVG drawHexPixels
  spriteE hex $ overBeginning stdFade fadeInE

  -- Show colormap  
  waitUntil $ wordStart $ findWord [] "if"
  cmapT <- newVar 0
  cmap  <- newSprite $ showColorMap 0 1 <$> unVar cmapT
  spriteE cmap $ overBeginning stdFade fadeInE
  spriteE cmap $ overEnding stdFade fadeOutE

  waitUntil $ wordStart $ findWord [] "and"
  tweenVar cmapT 2 $ \v -> fromToS v 1 . curveS 2
  wait 1 >> destroySprite cmap

  waitUntil $ wordStart $ findWord [] "understandable"
  imgT <- newVar 0
  img  <- newSprite $ drawPixelImage minV maxV <$> unVar imgT
  let imgDur = wordEnd (findWord [] "monalisa")
        - wordStart (findWord [] "understandable")
  tweenVar imgT imgDur $ \v -> fromToS v 1 . curveS 2

  destroySprite hex

  waitUntil $ wordStart $ findWord ["colormap"] "Shades"
  destroySprite img
    -- Move monalisa to the side of the screen
  play sceneFalseColorIntro
 where
  PixelRGB8 minR _ _ = minPixel monalisa
  PixelRGB8 maxR _ _ = maxPixel monalisa
  minV               = fromIntegral minR / 255
  maxV               = (fromIntegral maxR + 1) / 255
  stdFade            = 0.3

falseColorScene :: Scene s ()
falseColorScene = spriteScope $ do

  delta <- newVar 0
  cms   <- newVar (greyscale, greyscale)
  let total = 7

  nth <- newVar 0
  let pushCM label cm = do
        (_, prevCM) <- readVar cms
        writeVar delta 0
        writeVar cms   (prevCM, cm)

        this <- readVar nth
        writeVar nth (this + 1)

        s <- newSpriteSVG $ drawColorMap label cm
        spriteE s (overBeginning 0.3 fadeInE)
        spriteTween s 0 $ \_ -> positionColorMap total this

        fork $ tweenVar delta 0.3 $ \v -> fromToS v 1 . curveS 2
  let cmSprite delta (cmap1, cmap2) t =
        let s = curveS 3 t
            cm x = interpolateRGB8 labComponents (cmap1 x) (cmap2 x) delta
        in  translate (screenWidth / 4 - 0.75) 0
              $ scaleToSize (screenWidth / 2) (screenHeight / 2)
              $ embedImage
              $ applyColorMap cm monalisa
  s <- newSprite $ cmSprite <$> unVar delta <*> unVar cms <*> spriteT

  pushCM "greyscale" greyscale

  waitUntil $ wordStart $ findWord ["colormap"] "sine"
  pushCM "sinebow" sinebow

  waitUntil $ wordStart $ findWord ["colormap"] "Jet"
  pushCM "jet, 1970s" jet

  waitUntil $ wordStart $ findWord ["colormap"] "Parula"
  pushCM "parula, 2014" parula

  waitUntil $ wordStart $ findWord ["colormap"] "Viridis"
  pushCM "viridis, 2015" viridis

  waitUntil $ wordStart $ findWord ["colormap"] "Cividis"
  pushCM "cividis, 2018" cividis

  waitUntil $ wordStart $ findWord ["colormap"] "Turbo"
  pushCM "turbo, 2019" turbo

  waitUntil $ wordEnd $ findWord ["why"] "theory"
  wait 1

maxPixel :: Image PixelRGB8 -> PixelRGB8
maxPixel img = pixelFold (\acc _ _ pix -> max acc pix) (pixelAt img 0 0) img

minPixel :: Image PixelRGB8 -> PixelRGB8
minPixel img = pixelFold (\acc _ _ pix -> min acc pix) (pixelAt img 0 0) img

-- RGB interpolation
interpolateColorMap
  :: Double
  -> (Double -> PixelRGB8)
  -> (Double -> PixelRGB8)
  -> (Double -> PixelRGB8)
interpolateColorMap d cmap1 cmap2 t =
  interpolateRGB8 xyzComponents (cmap1 t) (cmap2 t) d

sceneFalseColorIntro :: Animation
sceneFalseColorIntro = mkAnimation 2 $ \t ->
  let s = fromToS 1 2 $ curveS 3 t
      d = curveS 3 t
  in  mkGroup
        [ translate ((screenWidth / 4 - 0.75) * d) 0
        $ scaleToSize (screenWidth / s) (screenHeight / s)
        $ center
        $ embedImage monalisaLarge
        , withGroupOpacity d
        $ translate ((screenWidth / 4 - 0.75) * d) 0
        $ scaleToSize (screenWidth / s) (screenHeight / s)
        $ embedImage monalisa
        ]

sceneFalseColor :: (Double -> PixelRGB8) -> (Double -> PixelRGB8) -> Animation
sceneFalseColor cmap1 cmap2 = mkAnimation 5 $ \t ->
  let s  = curveS 3 t
      cm = interpolateColorMap s cmap1 cmap2
  in  translate (screenWidth / 4 - 0.75) 0
        $ scaleToSize (screenWidth / 2) (screenHeight / 2)
        $ embedImage
        $ applyColorMap cm monalisa

drawColorMap :: T.Text -> (Double -> PixelRGB8) -> SVG
drawColorMap label cmap = mkGroup
  [ renderColorMap width height cmap
  , withFillColor "white"
  $ translate (-width / 2 + 0.2) (height / 2 + 0.1)
  $ scale 0.3
  $ latex label
  ]
 where
  width  = screenWidth * 0.4
  height = screenHeight * 0.06

positionColorMap :: Int -> Int -> SVG -> SVG
positionColorMap total nth = translate xOffset
                                       (yInit - fromIntegral nth * yStep)
 where
  xOffset = -screenWidth * 0.3
  yInit   = yStep * fromIntegral (total `div` 2)
  yStep   = height * 2.2
  height  = screenHeight * 0.06


limitGreyPixels :: Word8 -> Image PixelRGB8 -> Image PixelRGBA8
limitGreyPixels limit = pixelMap $ \pixel@(PixelRGB8 r _ _) ->
  if r < limit then PixelRGBA8 r r r 255 else PixelRGBA8 0 0 0 0

renderColorMap :: Double -> Double -> (Double -> PixelRGB8) -> Tree
renderColorMap width height cmap = mkGroup
  [ scaleToSize width height $ mkColorMap cmap
  , center
  $ withStrokeWidth 0.01
  $ withStrokeColor "white"
  $ withFillOpacity 0
  $ mkRect width height
  ]

showColorMap :: Double -> Double -> Double -> SVG
showColorMap start end t = mkGroup
  [ withGroupOpacity 0.9 $ withFillColor "black" $ center $ mkRect
    (width + height * 2)
    (height * 3)
  , scaleToSize width height $ mkColorMap (cm . fromToS start end)
  , translate (t * width - width / 2) 0
  $ center
  $ withStrokeColor "black"
  $ mkLine (0, 0) (0, height)
  , center $ withStrokeColor "white" $ withFillOpacity 0 $ mkRect width height
  , translate (t * width - width / 2) height
  $ scale 0.3
  $ centerX
  $ withFillColorPixel (promotePixel $ cm n)
  $ withStrokeColor "white"
  $ getNthSet (round (n * 255) `div` stepSize * stepSize)
  ]
 where
  n        = fromToS start end t
  cm       = greyscale
  stepSize = 0x1
  width    = screenWidth * 0.50
  height   = screenHeight * 0.08
  ppHex n = T.pack $ reverse (take 2 (reverse (showHex n "") ++ repeat '0'))
  getNthSet n = snd (splitGlyphs [n * 2, n * 2 + 1] allGlyphs)
  allGlyphs =
    lowerTransformations
      $  scale 2
      $  center
      $  latex
      $  "\\texttt{"
      <> T.concat [ ppHex n <> "~" | n <- [0 .. 255] ]
      <> "}"

mkColorMap :: (Double -> PixelRGB8) -> Tree
mkColorMap f = center $ embedImage img
 where
  width  = 1000
  height = 1
  img    = generateImage pixelRenderer width height
  pixelRenderer x _y = f (fromIntegral x / fromIntegral width)


drawPixelImage :: Double -> Double -> Double -> SVG
drawPixelImage start end t =
  scaleToSize screenWidth screenHeight $ center $ embedImage $ cache !! idx
 where
  cache = [ limitGreyPixels n monalisaLarge | n <- [0 .. 255] ]
  idx   = floor (fromToS start end t * 255)

drawHexPixels :: SVG
drawHexPixels = prerenderSvg ("drawHexPixels" :: String) svg
 where
  svg = simplify $ simplify $ simplify $ mkGroup
    [ mkDefinitions images
    , withFillOpacity 1 $ withStrokeWidth 0 $ withFillColor "white" $ mkGroup
      [ translate
            ( (fromIntegral x + 0.5)
            / fromIntegral width
            * screenWidth
            - screenWidth
            / 2
            )
            ( screenHeight
            / 2
            - (fromIntegral y + 0.5)
            / fromIntegral height
            * screenHeight
            )
          $ mkUse ("tag" ++ show r)
      | x <- [0 .. width - 1]
      , y <- [0 .. height - 1]
      , let pixel@(PixelRGB8 r _ _) = pixelAt monalisa x y
      ]
    ]
  getNthSet n = centerX $ snd (splitGlyphs [n * 2, n * 2 + 1] allGlyphs)
  allGlyphs =
    lowerTransformations
      $  scale 0.15
      $  center
      $  latex
      $  "\\texttt{"
      <> T.concat [ ppHex n | n <- [0 .. 255] ]
      <> "}"
  images = [ withId ("tag" ++ show n) $ getNthSet n | n <- [0 .. 255] ]
  width  = imageWidth monalisa
  height = imageHeight monalisa
  ppHex n = T.pack $ reverse (take 2 (reverse (showHex n "") ++ repeat '0'))

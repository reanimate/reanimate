#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main
  ( main
  )
where

import           Control.Lens                             ( )
import           Control.Monad
import qualified Data.ByteString               as BS
import qualified Data.Map                      as Map
import           Data.Monoid
import qualified Data.Text                     as T

import           Codec.Picture
import           Codec.Picture.Jpg
import           Codec.Picture.Types
import           Data.Word
import           Graphics.SvgTree                  hiding ( Image
                                                          , imageHeight
                                                          , imageWidth
                                                          )
import           Graphics.SvgTree.Memo
import           Numeric
import           Reanimate
import           Reanimate.Animation
import           Reanimate.ColorComponents
import           Reanimate.ColorMap
import           Reanimate.ColorSpace
import           Reanimate.Builtin.Flip
import           Reanimate.Builtin.Slide
import           Reanimate.Constants
import           Reanimate.Effect
import           Reanimate.LaTeX
import           Reanimate.Raster
import           Reanimate.Scene
import           Reanimate.Ease
import           Reanimate.Transition
import           Reanimate.Svg
import           System.IO.Unsafe

import           Grid
import           Spectrum
import           EndScene
import           Transcript

{- Scene sequence

 - black
 - numbers
 - monalisa
 - monalisa to right side
 - colormaps added to left side
 - screen slide transition
 - draw LMS sensitivities
 - morph to XYZ sensitivities
 - move graph to right side
 - fade in ternary plot
 - move over XYZ labels
 - move over spectrum line
 - hide spectrum & center ternary plot
 - cut to visible colors
 - draw sRGB triangle
 - cut to sRGB triangle
 - scale sRGB triangle to ternary plot
 - show interpolation between green and red
 - it has yellow in the middle because xyz-space is based on physical reality
 - show interpolation between cyan and red
 - show that grey is in the middle
 - transition xyz-space to hsv-space
-}

highdef = False

-- beginWord = findWord ["why"] "theory"
-- beginWord = findWord ["theory"] "spectrum"
-- beginWord = findWord ["simplify"] "awkward"
-- beginWord = findWord ["xyz"] "squeezed"
-- beginWord = findWord ["xyz"] "Taking"
-- beginWord = findWord ["rgb"] "Finally"
-- beginWord = findWord ["hsv"] "we"
-- beginWord = findWord ["lab"] "LAB"
beginWord = findWord ["made"] "instead"

-- endWord = findWord ["xyz"] "colors"
-- endWord = findWord ["simplify"] "All"
-- endWord = findWord ["xyz"] "sRGB"
-- endWord = findWord ["pyramid"] "approximations"
-- endWord = findWord ["hsv"] "saturation"
endWord = findWord ["lab"] "THEEND"

main :: IO ()
main =
  reanimate
    -- $ dropA (wordStart beginWord)
    -- $ takeA (wordEnd endWord)
    $ sceneAnimation
    $ do
        newSpriteSVG_ $ mkBackground "black"
        fork $ adjustZ (\x -> x + 100) $ annotateWithTranscript transcript
        monalisaScene
        transitionO transition1 1 falseColorScene $ do
          transitionO transition2 1 scene2 $ do
            transitionO transition3 2 gridScene endScene
 where
  transition1 = signalT (curveS 2) slideDownT
  transition2 = signalT (curveS 2) slideUpT
  transition3 = signalT (curveS 2) slideDownT -- flipTransition

monalisaScene :: Scene s ()
monalisaScene = spriteScope $ do
  -- Fade in numbers
  waitUntil $ wordStart $ findWord [] "wall"
  hex <- newSpriteSVG drawHexPixels
  spriteE hex $ overBeginning stdFade fadeInE

  -- Show colormap  
  waitUntil $ wordStart $ findWord [] "if"
  cmapT <- newVar 0
  cmap  <- newSprite $ showColorMap minV maxV <$> unVar cmapT
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

  -- play
  --     $ drawPixelImage (fromIntegral minR / 255) ((fromIntegral maxR + 1) / 255)
  --     # setDuration toGrayScaleTime
  --     # pauseAround drawPixelDelay 3
  destroySprite hex

  waitUntil $ wordStart $ findWord ["colormap"] "Using"
  destroySprite img
    -- Move monalisa to the side of the screen
  play $ sceneFalseColorIntro
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

  waitUntil $ wordStart $ findWord ["colormap"] "sinebow"
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
  return ()



monalisa :: Image PixelRGB8
monalisa = unsafePerformIO $ do
  dat <- BS.readFile "monalisa.jpg"
  case decodeJpeg dat of
    Left  err -> error err
    Right img -> return $ convertRGB8 img

monalisaLarge :: Image PixelRGB8
monalisaLarge = scaleImage (if highdef then 15 else 1) monalisa

maxPixel :: Image PixelRGB8 -> PixelRGB8
maxPixel img = pixelFold (\acc _ _ pix -> max acc pix) (pixelAt img 0 0) img

minPixel :: Image PixelRGB8 -> PixelRGB8
minPixel img = pixelFold (\acc _ _ pix -> min acc pix) (pixelAt img 0 0) img

scaleImage :: Pixel a => Int -> Image a -> Image a
scaleImage factor img = generateImage fn
                                      (imageWidth img * factor)
                                      (imageHeight img * factor)
  where fn x y = pixelAt img (x `div` factor) (y `div` factor)

applyColorMap :: (Double -> PixelRGB8) -> Image PixelRGB8 -> Image PixelRGB8
applyColorMap cmap img = generateImage fn (imageWidth img) (imageHeight img)
 where
  fn x y = case pixelAt img x y of
    PixelRGB8 r _ _ -> cmap (fromIntegral r / 255)

-- RGB interpolation
interpolateColorMap
  :: Double
  -> (Double -> PixelRGB8)
  -> (Double -> PixelRGB8)
  -> (Double -> PixelRGB8)
interpolateColorMap d cmap1 cmap2 = \t ->
  let PixelRGB8 r1 g1 b1 = cmap1 t
      PixelRGB8 r2 g2 b2 = cmap2 t
      i a b = round (fromIntegral a + (fromIntegral b - fromIntegral a) * d)
  in  PixelRGB8 (i r1 r2) (i g1 g2) (i b1 b2)

sceneFalseColorChain (x : y : xs) =
  sceneFalseColor x y `seqA` sceneFalseColorChain (y : xs)
sceneFalseColorChain _ = pause 0

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
showColorMap start end t =
  let s = t
      n = fromToS start end s
  in  translate 0 offsetY $ mkGroup
        [ withGroupOpacity 0.9
        $ withFillColor "black"
        $ translate 0 (0)
        $ center
        $ mkRect (width + height * 2) (height * 3)
        , scaleToSize width height $ mkColorMap (cm . fromToS start end)
        , translate (s * width - width / 2) 0
        $ center
        $ withStrokeColor "black"
        $ mkLine (0, 0) (0, height)
        , center
        $ --withStrokeWidth (Num 0.5) $
          withStrokeColor "white"
        $ withFillOpacity 0
        $ mkRect width height
        , translate (s * width - width / 2) (height)
        $ scale 0.3
        $ centerX
        $
      -- withStrokeWidth (Num 0.2) $
          withFillColorPixel (promotePixel $ cm n)
        $ withStrokeColor "white"
        $ getNthSet (round (n * 255) `div` stepSize * stepSize)
        ]
 where
  cm       = greyscale
  offsetY  = 0 -- -screenHeight * 0.30
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
  let limit = fromToS start end t
  in  scaleToSize screenWidth screenHeight
        $  center
        $  embedImage
        $  cache
        !! floor (limit * 255)
  where cache = [ limitGreyPixels n monalisaLarge | n <- [0 .. 255] ]

drawHexPixels :: SVG
drawHexPixels = svg
 where
  svg = -- scaleToSize screenWidth screenHeight $ embedDynamicImage $ raster $
        simplify $ simplify $ simplify $ mkGroup
    [ if True || highdef then defs else None
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
          $ if True || highdef then mkUse ("tag" ++ show r) else mkCircle 0.5
      | x <- [0 .. width - 1]
      , y <- [0 .. height - 1]
      , let pixel@(PixelRGB8 r _ _) = pixelAt monalisa x y
      ]
    ]
  defs = preRender $ mkDefinitions images
  getNthSet n = centerX $ snd (splitGlyphs [n * 2, n * 2 + 1] allGlyphs)
  allGlyphs =
    lowerTransformations
      $  scale 0.15
      $  center
      $  latex
      $  "\\texttt{"
      <> T.concat [ ppHex n | n <- [0 .. 255] ]
      <> "}"
  images =
    [ withId ("tag" ++ show n) $ getNthSet n
      -- lowerTransformations $ scale 0.3 $ alignTxt $ latex $ "\\texttt{" <> ppHex n <> "}"
    | n <- [0 .. 255]
    ]
  width  = imageWidth monalisa
  height = imageHeight monalisa
  ppHex n = T.pack $ reverse (take 2 (reverse (showHex n "") ++ repeat '0'))


fadeIn :: Double -> Animation -> Animation
fadeIn t = applyE (overBeginning t fadeInE)

fadeOut :: Double -> Animation -> Animation
fadeOut t = applyE (overEnding t fadeOutE)

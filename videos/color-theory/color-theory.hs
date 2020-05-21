#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Control.Lens          ()
import           Control.Monad
import qualified Data.ByteString       as BS
import qualified Data.Map              as Map
import           Data.Monoid
import qualified Data.Text             as T

import           Codec.Picture
import           Codec.Picture.Jpg
import           Codec.Picture.Types
import           Data.Word
import           Graphics.SvgTree      hiding (Image, imageHeight, imageWidth)
import           Graphics.SvgTree.Memo
import           Numeric
import           Reanimate
import           Reanimate.Animation
import           Reanimate.ColorMap
import           Reanimate.ColorSpace
import           Reanimate.Builtin.Flip
import           Reanimate.Constants
import           Reanimate.Effect
import           Reanimate.LaTeX
import           Reanimate.Raster
import           Reanimate.Scene
import           Reanimate.Ease
import           Reanimate.Transition
import           Reanimate.Interpolate
import           Reanimate.Svg
import           System.IO.Unsafe

import           Grid
import           Spectrum
import           EndScene
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

highdef = True

main :: IO ()
main = reanimate $ --  takeA 10 $ dropA 55 $
  parA (staticFrame 1 $ mkBackground "black") $
  monalisaScene `seqA`
  falseColorScene `seqA`
  scene2 `seqA`
  (parA (staticFrame 1 $ mkBackground "aliceblue") $
  overlapT 2 (signalT (curveS 2) flipTransition)
    (parA (staticFrame 1 $ mkBackground "black") $ gridScene)
    (parA (staticFrame 1 $ mkBackground "black") $ endScene))
  -- scene3
  -- xyzTernaryPlot
  -- interpolation
  -- spacesA
  -- clipPathTest


monalisaScene :: Animation
monalisaScene =
  sceneAnimation (do
    wait blackIntro
    -- Draw numbers
    fork $
      play $ drawHexPixels
        # setDuration (drawPixelDelay+toGrayScaleTime+3)
        # pauseAtBeginning beginPause
        # fadeIn stdFade
    wait beginPause
    -- Schedule monalisa fade-in
    let PixelRGB8 minR _ _ = minPixel monalisa
        PixelRGB8 maxR _ _ = maxPixel monalisa
    waitOn $ do
      -- Show colormap as monalisa fades in
      play $ showColorMap (fromIntegral minR/255) ((fromIntegral maxR+1)/255)
        # setDuration 2
        # pauseAround 1 1
        # fadeIn stdFade
        # fadeOut stdFade

      play $ drawPixelImage (fromIntegral minR/255) ((fromIntegral maxR+1)/255)
        # setDuration toGrayScaleTime
        # pauseAround drawPixelDelay 3
      -- Move monalisa to the side of the screen
      play $ sceneFalseColorIntro



    -- Cycle through colormaps for monalisa
    -- play $ sceneColorMaps `sim` (sceneFalseColorChain $ map snd
    --   [ ("Greyscale", greyscale)
    --   , ("Jet", jet)
    --   , ("Turbo", turbo)
    --   , ("Viridis", viridis)
    --   , ("Parula", parula)
    --   , ("Plasma", plasma)
    --   , ("Cividis", cividis)
    --   ])
    return ()
  )
  where
    blackIntro = 2
    beginPause = 4
    drawPixelDelay = 1
    stdFade = 0.3
    toGrayScaleTime = 3

falseColorScene :: Animation
falseColorScene = sceneAnimation $ do

    delta <- newVar 0
    cms <- newVar (greyscale, greyscale)
    let total = 7

    nth <- newVar 0
    let pushCM label cm = do
          (_, prevCM) <- readVar cms
          writeVar delta 0
          writeVar cms (prevCM, cm)

          this <- readVar nth
          writeVar nth (this+1)

          s <- fork $ newSpriteA $ drawColorMap label cm
          spriteE s (overBeginning 0.3 fadeInE)
          spriteTween s 0 $ \_ -> positionColorMap total this

          fork $ tweenVar delta 0.3 $ \v -> fromToS v 1 . curveS 2
          wait 3
    let cmSprite delta (cmap1, cmap2) t =
          let s = curveS 3 t
              cm x = interpolateRGB8 labComponents (cmap1 x) (cmap2 x) delta
          in translate (screenWidth/4 - 0.75) 0 $
             scaleToSize (screenWidth/2) (screenHeight/2) $
             embedImage $ applyColorMap cm monalisa
    s <- newSprite $
      cmSprite
        <$> unVar delta
        <*> unVar cms
        <*> spriteT

    pushCM "greyscale" greyscale
    pushCM "jet" jet
    pushCM "turbo" turbo
    pushCM "sinebow" sinebow
    pushCM "parula" parula
    pushCM "viridis" viridis
    pushCM "cividis" cividis

    wait 2
    return ()



monalisa :: Image PixelRGB8
monalisa = unsafePerformIO $ do
  dat <- BS.readFile "monalisa.jpg"
  case decodeJpeg dat of
    Left err  -> error err
    Right img -> return $ convertRGB8 img

monalisaLarge :: Image PixelRGB8
monalisaLarge = scaleImage (if highdef then 15 else 1) monalisa

maxPixel :: Image PixelRGB8 -> PixelRGB8
maxPixel img = pixelFold (\acc _ _ pix -> max acc pix) (pixelAt img 0 0) img

minPixel :: Image PixelRGB8 -> PixelRGB8
minPixel img = pixelFold (\acc _ _ pix -> min acc pix) (pixelAt img 0 0) img

scaleImage :: Pixel a => Int -> Image a -> Image a
scaleImage factor img =
    generateImage fn (imageWidth img * factor) (imageHeight img * factor)
  where
    fn x y = pixelAt img (x `div` factor) (y `div` factor)

applyColorMap :: (Double -> PixelRGB8) -> Image PixelRGB8 -> Image PixelRGB8
applyColorMap cmap img =
    generateImage fn (imageWidth img) (imageHeight img)
  where
    fn x y =
      case pixelAt img x y of
        PixelRGB8 r _ _ -> cmap (fromIntegral r/255)

-- RGB interpolation
interpolateColorMap :: Double -> (Double -> PixelRGB8) -> (Double -> PixelRGB8)
  -> (Double -> PixelRGB8)
interpolateColorMap d cmap1 cmap2 =
  \t ->
    let PixelRGB8 r1 g1 b1 = cmap1 t
        PixelRGB8 r2 g2 b2 = cmap2 t
        i a b = round (fromIntegral a + (fromIntegral b-fromIntegral a)*d)
    in PixelRGB8 (i r1 r2) (i g1 g2) (i b1 b2)

sceneFalseColorChain (x:y:xs) = sceneFalseColor x y `seqA` sceneFalseColorChain (y:xs)
sceneFalseColorChain _ = pause 0

sceneFalseColorIntro :: Animation
sceneFalseColorIntro = mkAnimation 2 $ \t ->
  let s = fromToS 1 2 $ curveS 3 t
      d  = curveS 3 t
  in mkGroup
  [ translate ((screenWidth/4 - 0.75)*d) 0 $
    scaleToSize (screenWidth/s) (screenHeight/s) $
    center $ embedImage monalisaLarge
  , withGroupOpacity d $
    translate ((screenWidth/4 - 0.75)*d) 0 $
    scaleToSize (screenWidth/s) (screenHeight/s) $
    embedImage monalisa ]

sceneFalseColor :: (Double -> PixelRGB8) -> (Double -> PixelRGB8) -> Animation
sceneFalseColor cmap1 cmap2 = mkAnimation 5 $ \t ->
  let s = curveS 3 t
      cm = interpolateColorMap s cmap1 cmap2
  in translate (screenWidth/4 - 0.75) 0 $
    scaleToSize (screenWidth/2) (screenHeight/2) $ embedImage $
    applyColorMap cm monalisa

sceneColorMaps :: Animation
sceneColorMaps = mkAnimation 5 $ const $
  mkGroup
    [ translate xOffset (yInit - n*yStep) $
      mkGroup
        [ renderColorMap width height cmap
        , withFillColor "white" $ translate (-width/2) (height*1) $
          scale 0.3 $ latex name
        ]
    | (n, (name, cmap)) <- zip [0..] maps ]
  where
    maps =
      [ ("Greyscale", greyscale)
      , ("Jet", jet)
      , ("Turbo", turbo)
      , ("Viridis", viridis)
      , ("Parula", parula)
      , ("Plasma", plasma)
      , ("Cividis", cividis) ]
    xOffset = -screenWidth*0.3
    yInit = yStep*3
    yStep = height * 2.2
    width = screenWidth*0.4
    height = screenHeight*0.06

drawColorMap :: T.Text -> (Double -> PixelRGB8) -> Animation
drawColorMap label cmap = animate $ const $
  mkGroup
    [ renderColorMap width height cmap
    , withFillColor "white" $ translate (-width/2 + screenWidth*0.01) (height*1.1) $
      scale 0.3 $ latex label
    ]
  where
    width = screenWidth*0.4
    height = screenHeight*0.06

positionColorMap :: Int -> Int -> SVG -> SVG
positionColorMap total nth =
    translate xOffset (yInit - fromIntegral nth*yStep)
  where
    xOffset = -screenWidth*0.3
    yInit = yStep*fromIntegral (total `div` 2)
    yStep = height * 2.2
    height = screenHeight*0.06


limitGreyPixels :: Word8 -> Image PixelRGB8 -> Image PixelRGBA8
limitGreyPixels limit = pixelMap $ \pixel@(PixelRGB8 r _ _) ->
    if r < limit then PixelRGBA8 r r r 255 else PixelRGBA8 0 0 0 0

renderColorMap :: Double -> Double -> (Double -> PixelRGB8) -> Tree
renderColorMap width height cmap = mkGroup
  [ scaleToSize width height $ mkColorMap cmap
  , center $ withStrokeWidth 0.01 $
    withStrokeColor "white" $ withFillOpacity 0 $
    mkRect width height
  ]

showColorMap :: Double -> Double -> Animation
showColorMap start end = mkAnimation 2 $ \t ->
    let s = curveS 2 t
        n = fromToS start end s
    in translate 0 offsetY $
      mkGroup
      [ withGroupOpacity 0.9 $
        withFillColor "black" $
        translate 0 (0) $
        center $
        mkRect (width + height*2) (height*3)
      , scaleToSize width height $ mkColorMap (cm . fromToS start end)
      , translate (s*width - width/2) 0 $
        center $
        withStrokeColor "black" $
        mkLine (0, 0) (0, height)
      , center $ --withStrokeWidth (Num 0.5) $
        withStrokeColor "white" $ withFillOpacity 0 $
        mkRect width height
      , translate (s*width - width/2) (height) $
        scale 0.3 $
        centerX $
        -- withStrokeWidth (Num 0.2) $
        withFillColorPixel (promotePixel $ cm n) $
        withStrokeColor "white" $
        getNthSet (round (n*255) `div` stepSize * stepSize)
      ]
  where
    cm = greyscale
    offsetY = -screenHeight*0.30
    stepSize = 0x1
    width = screenWidth*0.50
    height = screenHeight*0.08
    ppHex n = T.pack $ reverse (take 2 (reverse (showHex n "") ++ repeat '0'))
    getNthSet n = snd (splitGlyphs [n*2,n*2+1] allGlyphs)
    allGlyphs = lowerTransformations $ scale 2 $ center $ latex $ "\\texttt{" <> T.concat
      [ ppHex n <> "~"
      | n <- [0..255]
      ] <> "}"

mkColorMap :: (Double -> PixelRGB8) -> Tree
mkColorMap f = center $ embedImage img
  where
    width = 1000
    height = 1
    img = generateImage pixelRenderer width height
    pixelRenderer x _y = f (fromIntegral x / fromIntegral width)


drawPixelImage :: Double -> Double -> Animation
drawPixelImage start end = mkAnimation 2 $ \t ->
    let limit = fromToS start end $ curveS 2 t
    in scaleToSize screenWidth screenHeight $ center $ embedImage $
       cache !! floor (limit*255)
  where
    cache =
      [ limitGreyPixels n monalisaLarge
      | n <- [0..255] ]

drawHexPixels :: Animation
drawHexPixels = mkAnimation 1 $ \_ -> svg
  where
    svg = -- scaleToSize screenWidth screenHeight $ embedDynamicImage $ raster $
      simplify $ simplify $ simplify $
        mkGroup
        [ if highdef then defs else None
        , withFillOpacity 1 $ withStrokeWidth 0 $ withFillColor "white" $
          mkGroup
          [ translate ((fromIntegral x+0.5)/fromIntegral width*screenWidth - screenWidth/2)
                      (screenHeight/2 - (fromIntegral y+0.5)/fromIntegral height*screenHeight) $
            if highdef
              then mkUse ("tag" ++ show r)
              else mkCircle 0.5
          | x <- [0..width-1]
          , y <- [0..height-1]
          , let pixel@(PixelRGB8 r _ _) = pixelAt monalisa x y
          ]
        ]
    defs = preRender $ mkDefinitions images
    getNthSet n = centerX $ snd (splitGlyphs [n*2,n*2+1] allGlyphs)
    allGlyphs = lowerTransformations $ scale 0.15 $ center $ latex $
      "\\texttt{" <> T.concat
      [ ppHex n
      | n <- [0..255]
      ] <> "}"
    images =
      [ withId ("tag"++show n) $
        getNthSet n
        -- lowerTransformations $ scale 0.3 $ alignTxt $ latex $ "\\texttt{" <> ppHex n <> "}"
      | n <- [0..255]
      ]
    width = imageWidth monalisa
    height = imageHeight monalisa
    ppHex n = T.pack $ reverse (take 2 (reverse (showHex n "") ++ repeat '0'))


fadeIn :: Double -> Animation -> Animation
fadeIn t = applyE (overBeginning t fadeInE)

fadeOut :: Double -> Animation -> Animation
fadeOut t = applyE (overEnding t fadeOutE)

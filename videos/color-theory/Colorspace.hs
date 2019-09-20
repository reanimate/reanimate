{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
module Colorspace (colorSpacesScene) where

import           Control.Lens                  ((&), (.~))

import           Codec.Picture
import           Data.Colour
import           Data.Colour.CIE
import           Data.Colour.CIE.Illuminant
import           Data.Colour.RGBSpace
import           Data.Colour.SRGB
import           Data.Colour.SRGB.Linear
import           Data.List
import qualified Data.Map                      as Map
import           Data.Ord
import           Data.Text                     (Text)
import           Graphics.SvgTree              hiding (Text)
import           Linear.V2
import qualified Reanimate.Builtin.TernaryPlot as Ternary
import           Reanimate.ColorMap
import           Reanimate.ColorSpace
import           Reanimate.Constants
import           Reanimate.Driver              (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Monad
import           Reanimate.Raster
import           Reanimate.Scene
import           Reanimate.Effect
import           Reanimate.Signal
import           Reanimate.Svg

labScaleX = 128
labScaleY = 128

blueName = "royalblue"
greenName = "green"
redName = "maroon"

colorSpacesScene :: Animation
colorSpacesScene = sceneAnimation $ mdo
    beginT <- queryNow
    fork $ play $ frame
      # setDuration (endT-beginT)
    fork $ playZ 1 $ mkAnimation 1 (emit spectrumGrid)
      # setDuration (endT-beginT)
    dur <- withSceneDuration $ do
      fork $ play $ drawSensitivity 1 short blueName
        # setDuration drawDur
        # pauseAtBeginning (drawStagger*0)
        # pauseUntil dur
      fork $ do
        wait (drawStagger*0 + drawDur)
        play $ drawLabel "S" 1 short blueName
          # setDuration 2
          # applyE (overBeginning 0.3 fadeInE)
      fork $ play $ drawSensitivity 1 medium greenName
        # setDuration drawDur
        # pauseAtBeginning (drawStagger*1)
        # pauseUntil dur
      fork $ do
        wait (drawStagger*1 + drawDur)
        play $ drawLabel "M" 1 medium greenName
          # setDuration 2
          # applyE (overBeginning 0.3 fadeInE)
      fork $ do
        wait (drawStagger*2 + drawDur)
        play $ drawLabel "L" 1 long redName
          # setDuration 2
          # applyE (overBeginning 0.3 fadeInE)
      play $ drawSensitivity 1 long redName
        # setDuration drawDur
        # pauseAtBeginning (drawStagger*2)
        # pauseAtEnd drawPause


    -- waitAll $ do
    --   fork $ play $ drawSensitivity 1 short "blue"
    --     # setDuration drawDur
    --     # reverseAnimation
    --   fork $ play $ drawSensitivity 1 medium "green"
    --     # setDuration drawDur
    --     # reverseAnimation
    --   fork $ play $ drawSensitivity 1 long "red"
    --     # setDuration drawDur
    --     # reverseAnimation

    dur2 <- withSceneDuration $ do

      fork $ do
        wait drawDur
        fork $ play $ drawLabel "Z" 2 zCoords blueName
          # setDuration 2
          # applyE (overBeginning 0.3 fadeInE)
        wait drawStagger
        fork $ play $ drawLabel "Y" 2 yCoords greenName
          # setDuration 2
          # applyE (overBeginning 0.3 fadeInE)
        wait drawStagger
        fork $ play $ drawLabel "X" 2 xCoords redName
          # setDuration 2
          # applyE (overBeginning 0.3 fadeInE)

      fork $ play $ morphSensitivity 1 2 short zCoords blueName
        # setDuration drawDur
        -- # pauseAtBeginning (drawStagger*0)
        # pauseUntil dur2
      fork $ play $ morphSensitivity 1 2 medium yCoords greenName
        # setDuration drawDur
        -- # pauseAtBeginning (drawStagger*1)
        # pauseUntil dur2
      play $ morphSensitivity 1 2 long xCoords redName
        # setDuration drawDur
        -- # pauseAtBeginning (drawStagger*2)
        # pauseAtEnd drawPause
    endT <- queryNow
    return ()
  where
    playAndFreezeUntil endT ani = do
      now <- queryNow
      fork $ play $ ani # pauseUntil (now-endT)
    drawDur = 3
    drawStagger = 1
    drawPause = 4
    long = [ (nm, l) | (nm, (l, m, s)) <- Map.toList coneSensitivity ]
    medium = [ (nm, m) | (nm, (l, m, s)) <- Map.toList coneSensitivity ]
    short = [ (nm, s) | (nm, (l, m, s)) <- Map.toList coneSensitivity ]

    xCoords = [ (nm, x) | (nm, (x, y, z)) <- Map.toList bigXYZCoordinates ]
    yCoords = [ (nm, y) | (nm, (x, y, z)) <- Map.toList bigXYZCoordinates ]
    zCoords = [ (nm, z) | (nm, (x, y, z)) <- Map.toList bigXYZCoordinates ]


frame = mkAnimation 2 $ do
    -- emit $ mkBackground "black"
    -- emit $ spectrumGrid
    s <- getSignal signalLinear
    let cm = hsv
    emit $ mkGroup
      [ mkClipPath "sRGB"
        [ simplify
          sRGBTriangle
        ]
      , mkClipPath "visible"
        [ simplify
          obsColors
        ]
      , mkClipPath "spectrum" $
        let margin = 1 in
        [ simplify $ lowerTransformations $
          translate 0 (margin/2) $
          pathify $
          mkRect (Num spectrumWidth) (Num $ spectrumHeight+margin)
        ]
      ]

    -- emit $ mkGroup
    --   [ translate (-screenWidth*0.2) 0 $ mkGroup
    --     [ -- withClipPathRef (Ref "sRGB") $
    --       --withClipPathRef (Ref "visible") $
    --       mkGroup [img1]
    --     , withStrokeColor "white" $
    --       sRGBTriangle
    --     , withStrokeColor "white" $
    --       obsColors
    --     ]
    --   , translate (screenWidth*0.2) 0 $ mkGroup
    --     [ -- withClipPathRef (Ref "sRGB") $
    --       withClipPathRef (Ref "sRGB") $
    --       mkGroup [img1]
    --     ]
    --   ]
  where
    imgSize :: Num a => a
    imgSize = 1000
    img1 = cieXYImage imgSize
    img2 = cieLABImage imgSize imgSize
    obsColors =
      lowerTransformations $
      scale 5 $
      renderXYZCoordinatesTernary
    labColors =
      lowerTransformations $
      scale (100/2) $
      renderLABCoordinates



renderXYZCoordinatesTernary :: Tree
renderXYZCoordinatesTernary =
  withFillOpacity 0 $
  mkLinePath $
  [ (x, y)
  | (_nm, (red,green,blue)) <- Map.toList lightXYZCoordinates
  , let (x,y) = Ternary.toOffsetCartesianCoords green red
  ]


cieXYImage :: Int -> Tree
cieXYImage density = Ternary.raster density $ \aCoord bCoord cCoord ->
    let RGB r g b = toSRGBBounded (cieXYZ aCoord bCoord cCoord)
    in PixelRGBA8 r g b 0xFF



cieLABImage :: Int -> Int -> Tree
cieLABImage width height = embedImage $ generateImage gen width height
  where
    gen x y =
      let
          aStar = (fromIntegral x / fromIntegral width) * labScaleX*2 - labScaleX
          bStar = (1-(fromIntegral y / fromIntegral height)) * labScaleY*2 - labScaleY
          lStar = findLStar aStar bStar
          RGB r g b = toSRGBBounded (cieLAB d65 lStar aStar bStar)
          -- RGB r g b = RGB (round $ lStar/100 * 255) (round $ lStar/100 * 255) (round $ lStar/100 * 255)
      in PixelRGB8 r g b

findLStar :: Double -> Double -> Double
findLStar aStar bStar = worker 0 100 10
  where
    -- worker minL maxL n | trace (show (minL, maxL, n)) False = undefined
    worker minL maxL 0 = minL + (maxL-minL)/2
    worker minL maxL n
      | total > 1 = worker minL thisL (n-1)
      | otherwise = worker thisL maxL (n-1)
      where
        thisL = minL + (maxL-minL)/2
        (x,y,z) = cieXYZView (cieLAB d65 thisL aStar bStar)
        total = x+y+z

colorMapToLABCoords :: (Double -> PixelRGB8) -> Tree
colorMapToLABCoords colorMap = withFillOpacity 0 $ mkLinePath
    [ (aStar, 1-bStar)
    | n <- [0 .. steps]
    , let PixelRGB8 r g b = colorMap (n/steps)
          (lStar, aStar, bStar) = cieLABView d65 (sRGB24 r g b)
    ]
  where
    steps = 100

colorMapToXYZCoords :: (Double -> PixelRGB8) -> Tree
colorMapToXYZCoords colorMap = withFillOpacity 0 $ mkLinePath
    [ (x/s, 1-(y/s))
    | n <- [0 .. steps]
    , let PixelRGB8 r g b = colorMap (n/steps)
          (x, y, z) = cieXYZView (sRGB24 r g b)
          s = x+y+z
    ]
  where
    steps = 100

sRGBTriangle :: Tree
sRGBTriangle =
    lowerTransformations $
    scale 5 $
    withFillOpacity 0 $
    mkClosedLinePath
    [ Ternary.toOffsetCartesianCoords rY rX
    , Ternary.toOffsetCartesianCoords gY gX
    , Ternary.toOffsetCartesianCoords bY bX ]
  where
    RGB r g b = primaries sRGBGamut
    (rX, rY, _) = chromaCoords $ chromaConvert r
    (gX, gY, _) = chromaCoords $ chromaConvert g
    (bX, bY, _) = chromaCoords $ chromaConvert b

mkClosedLinePath :: [(Double, Double)] -> Tree
mkClosedLinePath [] = mkGroup []
mkClosedLinePath ((startX, startY):rest) =
    PathTree $ defaultSvg & pathDefinition .~ cmds
  where
    cmds = [ MoveTo OriginAbsolute [V2 startX startY]
           , LineTo OriginAbsolute [ V2 x y | (x, y) <- rest ]
           , EndPath ]

spectrumHeight = screenHeight * 0.5
spectrumWidth = screenWidth * 0.7

spectrumGrid :: Tree
spectrumGrid =
  withStrokeWidth (Num strokeWidth) $
  mkGroup
  [ center $
    withFillOpacity 0 $ withStrokeColor "white" $ mkPath $
    [ MoveTo OriginAbsolute [V2 0 0]
    , HorizontalTo OriginRelative [tickLength,-tickLength]
    , VerticalTo OriginRelative [-spectrumHeight]
    , HorizontalTo OriginRelative [spectrumWidth]
    , VerticalTo OriginRelative [tickLength,-tickLength]
    ]
    ++ concat
    [ [ MoveTo OriginAbsolute [V2 (n / (nTicksX-1) * spectrumWidth) (-spectrumHeight)]
      , VerticalTo OriginRelative [tickLength]]
    | n <- [0..nTicksX-1]
    ]
    ++ concat
    [ [ MoveTo OriginAbsolute [V2 0 (n / (nTicksY-1) * negate spectrumHeight)]
      , HorizontalTo OriginRelative [tickLength]]
    | n <- [0..nTicksY-1]
    ]
  , withFillColor "white" $
    translate (-spectrumWidth*0.5 - svgWidth sensitivity) (0) $
    sensitivity
  , withFillColor "white" $
    translate 0 (-spectrumHeight*0.5 -svgHeight wavelength) $
    wavelength
  , withFillColor "white" $
    translate (-spectrumWidth*0.5 + svgWidth shortWaves/2)
              (-spectrumHeight*0.5 -svgHeight shortWaves) $
    shortWaves
  , withFillColor "white" $
    translate (spectrumWidth*0.5 - svgWidth shortWaves/2)
              (-spectrumHeight*0.5 -svgHeight shortWaves) $
    longWaves
  ]
  where
    strokeWidth = 0.03
    nTicksX = 24
    nTicksY = fromIntegral (round (spectrumHeight/spectrumWidth * nTicksX))
    tickLength = spectrumHeight*0.02
    sensitivity =
      center $
      scale 0.4 $ rotate 90 $
      latex "Sensitivity $\\rightarrow$"
    wavelength =
      center $
      scale 0.4 $
      latex "Wavelength"
    shortWaves =
      center $
      scale 0.3 $
      latex "400 nm"
    longWaves =
      center $
      scale 0.3 $
      latex "700 nm"


drawSensitivity :: Double -> [(Nanometer, Double)] -> String -> Animation
drawSensitivity maxHeight dat c = mkAnimation 1 $ do
  limit <- getSignal signalLinear
  emit $ sensitivitySVG maxHeight limit dat c

morphSensitivity
  :: Double
  -> Double
  -> [(Nanometer, Double)]
  -> [(Nanometer, Double)]
  -> String
  -> Animation
morphSensitivity maxHeightA maxHeightB datA datB c = mkAnimation 1 $ do
  m <- getSignal $ signalCurve 2 -- signalLinear
  let maxHeight = signalFromTo maxHeightA maxHeightB id m
      dat = [ (nm, signalFromTo a b id m)
            | ((nm,a),(_,b)) <- zip datA datB ]
  emit $ sensitivitySVG maxHeight 1 dat c

  -- emit $
  --  withClipPathRef (Ref "spectrum") $
  --  simplify $ lowerTransformations $ pathify $
  --  mkBackground "green"
sensitivitySVG :: Double -> Double -> [(Nanometer, Double)] -> String -> Tree
sensitivitySVG maxHeight limit dat c =
    withClipPathRef (Ref "spectrum") $
    simplify $ lowerTransformations $
    withStrokeColor c $
    withFillOpacity 0 $
    translate (-spectrumWidth/2) (-spectrumHeight/2) $
    mkPath $ MoveTo OriginAbsolute [V2 0 0] :
      [ LineTo OriginAbsolute [V2 x y]
      | (nm, n) <- dat
      , fromIntegral nm <= lastNM
      , let percent = (fromIntegral nm-initNM)/(lastNM-initNM)
            x = percent * spectrumWidth
            y = n/maxHeight * spectrumHeight
      , percent <= limit
      ]
  where
    initNM = fromIntegral $ fst (head dat)
    lastNM = 700 -- fromIntegral $ fst (last dat)

drawLabel :: Text -> Double -> [(Nanometer, Double)] -> String -> Animation
drawLabel label maxHeight dat c = mkAnimation 1 $ do
  emit $
    translate (0) (svgHeight labelSVG) $
    translate (spectrumWidth*percent) labelY $
    translate (-spectrumWidth/2) (-spectrumHeight/2) $
    -- withStrokeWidth (Num 0.01) $
    -- withStrokeColor c $
    withFillColor c $
    labelSVG
  where
    labelSVG =
      center $
      scale 1 $
      latex label
    labelY = (v/maxHeight) * spectrumHeight
    (nm,v) = maximumBy (comparing snd) dat
    initNM = fromIntegral $ fst (head dat)
    lastNM = 700
    percent = (fromIntegral nm-initNM)/(lastNM-initNM)

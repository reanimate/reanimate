{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
module Spectrum
  ( colorSpacesScene
  , xyzTernaryPlot
  , interpolation
  , spacesA
  , scene2
  , scene3
  ) where

import           Control.Lens                    ((&), (.~))

import           Codec.Picture
import           Control.Monad
import           Data.Colour
import           Data.Colour.CIE
import           Data.Colour.CIE.Illuminant
import           Data.Colour.RGBSpace
import           Data.Colour.SRGB
import           Data.Colour.SRGB.Linear
import           Data.List
import qualified Data.Map                        as Map
import           Data.Ord
import           Data.Text                       (Text)
import           Graphics.SvgTree                hiding (Text)
import           Linear.V2
import           Reanimate
import           Reanimate.Animation
import           Reanimate.Builtin.Documentation
import qualified Reanimate.Builtin.TernaryPlot   as Ternary
import           Reanimate.ColorMap
import           Reanimate.ColorSpace
import           Reanimate.Constants
import           Reanimate.Driver                (reanimate)
import           Reanimate.Effect
import           Reanimate.Interpolate
import           Reanimate.LaTeX
import           Reanimate.Raster
import           Reanimate.Scene
import           Reanimate.Signal
import           Reanimate.Svg
import           Reanimate.Svg.BoundingBox

{- STORYBOARD
 - Wavelength scene
    * Background: wavelength/sensitivity grid
    * Draw SML sensitivities with labels
    * Hide labels
    * Morph to XYZ sensitivities
    * Label as XYZ
-}

long = [ (nm, l) | (nm, (l, m, s)) <- Map.toList coneSensitivity ]
medium = [ (nm, m) | (nm, (l, m, s)) <- Map.toList coneSensitivity ]
short = [ (nm, s) | (nm, (l, m, s)) <- Map.toList coneSensitivity ]
xCoords = [ (nm, x/2) | (nm, (x, y, z)) <- Map.toList bigXYZCoordinates ]
yCoords = [ (nm, y/2) | (nm, (x, y, z)) <- Map.toList bigXYZCoordinates ]
zCoords = [ (nm, z/2) | (nm, (x, y, z)) <- Map.toList bigXYZCoordinates ]

labScaleX = 128
labScaleY = 128

blueName = "royalblue"
greenName = "green"
redName = "maroon"

drawSensitivities :: Animation
drawSensitivities = sceneAnimation $ do
    bg <- newSpriteA $ staticFrame 0 spectrumGrid
    spriteZ bg 1

    forM_ [(short, blueName), (medium, greenName), (long, redName)] $
      \(dat,name) -> do
        fork $ newSpriteA' SyncFreeze $ drawSensitivity 1 dat name
          # setDuration drawDur
        wait drawStagger
  where
    drawDur = 3
    drawStagger = 1

drawMorphingSensitivities :: Animation
drawMorphingSensitivities = sceneAnimation $ do
    bg <- newSpriteA $ staticFrame 0 spectrumGrid
    spriteZ bg 1

    forM_ keys $ \(datA, datB, name) -> do
      fork $ newSpriteA $ morphSensitivity datA datB name
        # setDuration drawDur
  where
    keys = [ (short, zCoords, blueName)
           , (medium, yCoords, greenName)
           , (long, xCoords, redName)]
    drawDur = 3

colorSpacesScene :: Animation
colorSpacesScene = sceneAnimation $ mdo
    beginT <- queryNow
    fork $ play $ frame
      # setDuration (endT-beginT)
    fork $ playZ 1 $ animate (const spectrumGrid)
      # setDuration (endT-beginT)
    dur <- withSceneDuration $ do
      fork $ play $ drawSensitivity 1 short blueName
        # setDuration drawDur
        # pauseAtBeginning (drawStagger*0)
        # pauseUntil dur
      fork $ do
        wait (drawStagger*0 + drawDur)
        play $ drawLabel "S" blueName
          # setDuration 2
          # applyE (overBeginning 0.3 fadeInE)
      fork $ play $ drawSensitivity 1 medium greenName
        # setDuration drawDur
        # pauseAtBeginning (drawStagger*1)
        # pauseUntil dur
      fork $ do
        wait (drawStagger*1 + drawDur)
        play $ drawLabel "M" greenName
          # setDuration 2
          # applyE (overBeginning 0.3 fadeInE)
      fork $ do
        wait (drawStagger*2 + drawDur)
        play $ drawLabel "L" redName
          # setDuration 2
          # applyE (overBeginning 0.3 fadeInE)
      play $ drawSensitivity 1 long redName
        # setDuration drawDur
        # pauseAtBeginning (drawStagger*2)
        # pauseAtEnd drawPause

    dur2 <- withSceneDuration $ do

      fork $ do
        wait drawDur
        zLabel <- newSpriteA $ drawLabel "Z" blueName
        zLabelFade <- spriteVar zLabel 0 withGroupOpacity
        fork $ tweenVar zLabelFade 0.3 (\v -> fromToS v 1)

        wait drawStagger
        fork $ play $ drawLabel "Y" greenName
          # setDuration 2
          # applyE (overBeginning 0.3 fadeInE)
        wait drawStagger
        fork $ play $ drawLabel "X" redName
          # setDuration 2
          # applyE (overBeginning 0.3 fadeInE)

      fork $ play $ morphSensitivity short zCoords blueName
        # setDuration drawDur
        -- # pauseAtBeginning (drawStagger*0)
        # pauseUntil dur2
      fork $ play $ morphSensitivity medium yCoords greenName
        # setDuration drawDur
        -- # pauseAtBeginning (drawStagger*1)
        # pauseUntil dur2
      play $ morphSensitivity long xCoords redName
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

drawLabelS = drawLabel "S" blueName
drawLabelM = drawLabel "M" greenName
drawLabelL = drawLabel "L" redName

drawLabelZ = drawLabel "Z" blueName
drawLabelY = drawLabel "Y" greenName
drawLabelX = drawLabel "X" redName

scene2 :: Animation
scene2 =  dropA 39 $ sceneAnimation $ do
  -- SML labels and timings.
  labelS <- fork $ newSpriteA $ drawLabelS
    # applyE (overBeginning 0.3 fadeInE)
    # applyE (overEnding 0.2 fadeOutE)
    # mapA (uncurry translate (labelPosition short))
  labelM <- fork $ do
    wait 2
    newSpriteA $ drawLabelM
      # applyE (overBeginning 0.3 fadeInE)
      # applyE (overEnding 0.2 fadeOutE)
      # mapA (uncurry translate (labelPosition medium))
  labelL <- fork $ do
    wait 3.5
    newSpriteA $ drawLabelL
      # applyE (overBeginning 0.3 fadeInE)
      # applyE (overEnding 0.2 fadeOutE)
      # mapA (uncurry translate (labelPosition long))

  play $ drawSensitivities
    # pauseAtEnd 1

  -- Drop SML labels
  forM_ [labelS, labelM, labelL] destroySprite

  xyzGraph <- fork $ newSpriteA' SyncFreeze $ drawMorphingSensitivities

  -- XYZ labels and timings
  wait (duration drawMorphingSensitivities)
  labelZ <- fork $ newSpriteA $ drawLabelZ
    # applyE (overBeginning 0.3 fadeInE)
  labelZPos <- spriteVar labelZ (labelPosition zCoords) $ uncurry translate

  labelY <- fork $ newSpriteA $ drawLabelY
    # applyE (overBeginning 0.3 fadeInE)
  labelYPos <- spriteVar labelY (labelPosition yCoords) $ uncurry translate

  labelX <- fork $ newSpriteA $ drawLabelX
    # applyE (overBeginning 0.3 fadeInE)
  labelXPos <- spriteVar labelX (labelPosition xCoords) $ uncurry translate

  wait 4

  fork $ spriteTween xyzGraph 2 $ \t -> scale (fromToS 1 0.5 $ curveS 2 t)
  fork $ spriteTween xyzGraph 2 $ \t -> translate (fromToS 0 (screenWidth/4) $ curveS 2 t) 0

  fork $ tweenVar labelZPos 2 $ \(x,y) t ->
    let (newX, newY) = Ternary.toOffsetCartesianCoords 0 0
        s = curveS 2 t
    in (fromToS x (newX*5-screenWidth/4) s, fromToS y (newY*5-1) s)

  fork $ tweenVar labelYPos 2 $ \(x,y) t ->
    let (newX, newY) = Ternary.toOffsetCartesianCoords 1 0
        s = curveS 2 t
    in (fromToS x (newX*5-screenWidth/4) s, fromToS y (newY*5) s)

  fork $ tweenVar labelXPos 2 $ \(x,y) t ->
    let (newX, newY) = Ternary.toOffsetCartesianCoords 0 1
        s = curveS 2 t
    in (fromToS x (newX*5-screenWidth/4) s, fromToS y (newY*5-1) s)

  wait 2

  redFactor <- newVar 0
  greenFactor <- newVar 0
  blueFactor <- newVar 0
  xyzSpace <- newSprite $ do
    getRed <- freezeVar redFactor
    getGreen <- freezeVar greenFactor
    getBlue <- freezeVar blueFactor
    return $ \real_t d t ->
      cieXYImage (getRed real_t) (getGreen real_t) (getBlue real_t) imgSize
  -- xyzSpace <- newSparite $ cieXYImage <$> unVar redFactor <*> unVar greenFactor <*> unVar blueFactor
  spriteE xyzSpace $ constE $ translate (-screenWidth/4) 0
  spriteZ xyzSpace (-1)



  spriteE labelX (overBeginning 1 $ aroundCenterE highlightE)
  tweenVar redFactor 1 $ \v -> fromToS v 1 . curveS 2

  spriteE labelY (overBeginning 1 $ aroundCenterE highlightE)
  tweenVar greenFactor 1 $ \v -> fromToS v 1 . curveS 2

  spriteE labelZ (overBeginning 1 $ aroundCenterE highlightE)
  tweenVar blueFactor 1 $ \v -> fromToS v 1 . curveS 2

  wait 3
  visLine <- fork $ newSpriteA' SyncFreeze $ mkAnimation 3 $ \t ->
    withStrokeWidth 0.03 $ withStrokeColor "white" $ morphXYZCoordinates t
  spriteZ visLine (1)
  visSide <- spriteVar visLine 0 $ \t ->
    translate (fromToS (screenWidth/4) (-screenWidth/4) $ curveS 2 t) 0 .
    scale (fromToS 0.5 1 $ curveS 2 t) .
    translate 0 (fromToS (-spectrumHeight/2) 0 $ curveS 2 t)
  fork $ tweenVar visSide 3 $ \v -> fromToS v 1

  obsVisible <- newVar 1
  gamut <- newVar 0
  visSpace <- fork $ newSprite $ do
    getObs <- freezeVar obsVisible
    getGamut <- freezeVar gamut
    return $ \real_t d t ->
      translate (-screenWidth/4) 0 $
      mkGroup
      [ mkClipPath "visible"
        [ simplify
          obsColors
        ]
      , mkClipPath "sRGB"
        [ simplify $
          sRGBTriangle (getGamut real_t)
        ]
      , withGroupOpacity (getObs real_t) $
        withClipPathRef (Ref "visible") $
        mkGroup [cieXYImage 1 1 1 imgSize]
      , withClipPathRef (Ref "sRGB") $
        mkGroup [cieXYImageGamut (getGamut real_t) imgSize]
      ]



  wait 4
  fork $ spriteTween visLine 1 $ \t -> withGroupOpacity (1-t)
  fork $ spriteTween xyzSpace 1 $ \t -> withGroupOpacity (1-t)
  wait 2

  fork $ spriteTween xyzGraph 1 $ \t -> withGroupOpacity (1-t)

  fork $ spriteTween labelX 1 $ \t -> withGroupOpacity (1-t)
  fork $ spriteTween labelY 1 $ \t -> withGroupOpacity (1-t)
  fork $ spriteTween labelZ 1 $ \t -> withGroupOpacity (1-t)

  wait 1

  spriteTween visSpace 1 $ \t ->
    translate (fromToS 0 (screenWidth/4) $ curveS 2 t) 0

  wait 2

  rgb <- newSprite $ do
    getGamut <- freezeVar gamut
    return $ \real_t d t ->
      withStrokeColor "white" $ sRGBTriangle (getGamut real_t)
  spriteTween rgb 1 $ partialSvg
  wait 1
  tweenVar obsVisible 1 $ \v -> fromToS v 0 . curveS 2
  wait 1
  fork $ spriteTween rgb 1 $ \t -> withGroupOpacity (1-t)
  tweenVar gamut 1 $ \v -> fromToS v 1 . curveS 2
  wait 2

  spriteTween visSpace 1 $ \t -> translate (-3*curveS 2 t) 0

  hsv <- newSpriteA $ animate $ const $
    scaleToSize (screenWidth/3) (screenHeight/3) $
    -- hsvColorSpace 100 100
    -- lchColorSpace 100 100
    -- cieLABImage 100 100
    cieLABImage 1000 1000

  spriteTween hsv 0 $ const $ translate 3 0

  return ()
  where
    imgSize = 50
    obsColors =
      lowerTransformations $
      scale 5 $
      renderXYZCoordinatesTernary

scene3 :: Animation
scene3 = sceneAnimation $ do
    play $ mkAnimation 1 $ \t -> cieXYImage 0 0 t imgSize
    play $ mkAnimation 1 $ \t -> cieXYImage t 0 1 imgSize
    play $ mkAnimation 1 (\t -> cieXYImage 1 t 1 imgSize)
      # pauseAtEnd 2
  where
    imgSize :: Num a => a
    imgSize = 300
    img1 t = cieXYImage t t t imgSize
    obsColors =
      lowerTransformations $
      scale 5 $
      renderXYZCoordinatesTernary
    labColors =
      lowerTransformations $
      scale (100/2) $
      renderLABCoordinates

frame = mkAnimation 2 $ \t ->
    -- emit $ mkBackground "black"
    -- emit $ spectrumGrid
    let s = t
        cm = hsv
    in mkGroup
      [ mkClipPath "sRGB"
        [ simplify $
          sRGBTriangle 0
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
          mkRect spectrumWidth (spectrumHeight+margin)
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
    img1 = cieXYImage 1 1 1 imgSize
    img2 = cieLABImage imgSize imgSize
    obsColors =
      lowerTransformations $
      scale 5 $
      renderXYZCoordinatesTernary
    labColors =
      lowerTransformations $
      scale (100/2) $
      renderLABCoordinates

xyzTernaryPlot = mkAnimation 2 $ \t ->
    -- emit $ mkBackground "black"
    -- emit $ spectrumGrid
    let s = t
        cm = hsv
    in mkGroup
      [ mkClipPath "sRGB"
        [ simplify $
          sRGBTriangle 0
        ]
      , mkClipPath "sRGBGamut"
        [ simplify $
          sRGBTriangle t
        ]
      , mkClipPath "visible"
        [ simplify
          obsColors
        ]
      , translate (-screenWidth*0.2) 0 $ mkGroup
        [ -- withClipPathRef (Ref "sRGB") $
          --withClipPathRef (Ref "visible") $
          mkGroup [img1 t]
        , withStrokeColor "white" $
          sRGBTriangle 0
        , withStrokeColor "white" $
          obsColors
        ]
      , translate (screenWidth*0.2) 0 $ mkGroup
        [ withClipPathRef (Ref "sRGBGamut") $
          --withClipPathRef (Ref "visible") $
          mkGroup [cieXYImageGamut t imgSize]
        ]
      -- , translate (screenWidth*0.2) 0 $ mkGroup
      --   [ -- withClipPathRef (Ref "sRGB") $
      --     withClipPathRef (Ref "sRGB") $
      --     mkGroup [img1 t]
      --   ]
      ]
  where
    imgSize :: Num a => a
    imgSize = 50
    img1 t = cieXYImage t t t imgSize
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

morphXYZCoordinates :: Double -> Tree
morphXYZCoordinates t = withFillOpacity 0 $
  mkLinePath $
  [ (fromToS linearX (x*5) t, fromToS 0 (y*5) t)
  | (nm, (red,green,blue)) <- Map.toList lightXYZCoordinates
  , let (x,y) = Ternary.toOffsetCartesianCoords green red
  , let percent = (fromIntegral nm-initNM)/(lastNM-initNM)
        linearX = percent * spectrumWidth - spectrumWidth/2
  ]
  where
    dat = Map.toList lightXYZCoordinates
    initNM = fromIntegral $ fst (head dat)
    lastNM = 700 -- fromIntegral $ fst (last dat)

-- Red corner: 0.64 0.33 0
-- Green corner: 0.3 0.6 0.1
-- Blue corner: 0.15 0.06 0.79
--1 0 0 -> 0.64 0.33 0.0
--0 1 0 -> 0.30 0.60 0.1
cieXYImageGamut :: Double -> Int -> Tree
cieXYImageGamut t density = Ternary.ternaryPlot density $ \aCoord bCoord cCoord ->
    let
      aCoord' = fromToS aCoord (rX * aCoord + gX * bCoord + bX * cCoord) t
      bCoord' = fromToS bCoord (rY * aCoord + gY * bCoord + bY * cCoord) t
      cCoord' = fromToS cCoord (rZ * aCoord + gZ * bCoord + bZ * cCoord) t
      RGB r g b = toSRGBBounded (cieXYZ aCoord' bCoord' cCoord)
    in PixelRGBA8 r g b 0xFF
  where
    RGB r g b = primaries sRGBGamut
    (rX, rY, rZ) = chromaCoords $ chromaConvert r
    (gX, gY, gZ) = chromaCoords $ chromaConvert g
    (bX, bY, bZ) = chromaCoords $ chromaConvert b

-- aCoord = red
-- bCoord = green
-- cCoord = blue
-- closer to white when (aCoord+cCoord) approaches 0
cieXYImage :: Double -> Double -> Double -> Int -> Tree
cieXYImage redFactor greenFactor blueFactor density =
    Ternary.ternaryPlot density $ \aCoord bCoord cCoord ->
    let
      white = chromaColour d65 1
      c = cieXYZ aCoord bCoord cCoord
      redD = fromToS (1-aCoord) 1 redFactor
      greenD = fromToS (1-bCoord) 1 greenFactor
      blueD = fromToS (1-cCoord) 1 blueFactor
      RGB r g b = toSRGBBounded $
        blend (1-redD) white $
        blend (1-greenD) white $
        blend (1-blueD) white $ c
      -- RGB r g b = toSRGBBounded (chromaColour d65 1)
    in PixelRGBA8 r g b 0xFF

cieLABImage :: Int -> Int -> Tree
cieLABImage width height = embedImage $ generateImage gen width height
  where
    gen x y =
      let
          aStar = (fromIntegral x / fromIntegral width) * labScaleX*2 - labScaleX
          bStar = (1-(fromIntegral y / fromIntegral height)) * labScaleY*2 - labScaleY
          lStar = 50 -- findLStar aStar bStar
          color = cieLAB d65 lStar aStar bStar
          RGB r g b = toSRGBBounded (cieLAB d65 lStar aStar bStar)
          -- RGB r g b = RGB (round $ lStar/100 * 255) (round $ lStar/100 * 255) (round $ lStar/100 * 255)
      in if inGamut sRGBGamut color
          then PixelRGBA8 r g b 0xFF
          else PixelRGBA8 r g b 0x00

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

sRGBTriangle :: Double -> Tree
sRGBTriangle t =
    lowerTransformations $
    scale 5 $
    withFillOpacity 0 $
    mkClosedLinePath
    [ Ternary.toOffsetCartesianCoords (fromToS rY 0 t) (fromToS rX 1 t)
    , Ternary.toOffsetCartesianCoords (fromToS gY 1 t) (fromToS gX 0 t)
    , Ternary.toOffsetCartesianCoords (fromToS bY 0 t) (fromToS bX 0 t) ]
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
  withStrokeWidth strokeWidth $
  mkGroup
  [ --center $
    translate (-spectrumWidth/2) (spectrumHeight/2) $
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
    translate (-spectrumWidth*0.5 + svgWidth sensitivity*1.2) 0 $
    sensitivity
  , withFillColor "white" $
    translate 0 (-spectrumHeight*0.5 + svgHeight wavelength*1.2) $
    wavelength
  -- , withFillColor "white" $
  --   translate (-spectrumWidth*0.5 + svgWidth shortWaves/2)
  --             (-spectrumHeight*0.5 + svgHeight shortWaves) $
  --   shortWaves
  -- , withFillColor "white" $
  --   translate (spectrumWidth*0.5 - svgWidth shortWaves/2)
  --             (-spectrumHeight*0.5 + svgHeight shortWaves) $
  --   longWaves
  ]
  where
    strokeWidth = 0.03
    nTicksX = 24
    nTicksY = fromIntegral (round (spectrumHeight/spectrumWidth * nTicksX))
    tickLength = spectrumHeight*0.02
    sensitivity =
      center $
      scale 0.7 $ rotate 90 $
      latex "Sensitivity $\\rightarrow$"
    wavelength =
      center $
      scale 0.8 $
      latex "Wavelength"
    shortWaves =
      center $
      scale 0.6 $
      latex "400 nm"
    longWaves =
      center $
      scale 0.6 $
      latex "700 nm"


drawSensitivity :: Double -> [(Nanometer, Double)] -> String -> Animation
drawSensitivity maxHeight dat c = animate $ \limit ->
  sensitivitySVG maxHeight limit dat c

morphSensitivity
  :: [(Nanometer, Double)]
  -> [(Nanometer, Double)]
  -> String
  -> Animation
morphSensitivity datA datB c = animate $ \t ->
  let m = curveS 2 t
      dat = [ (nm, fromToS a b m)
            | ((nm,a),(_,b)) <- zip datA datB ]
  in sensitivitySVG 1 1 dat c

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

labelPosition :: [(Nanometer, Double)] -> (Double, Double)
labelPosition dat =
    ( spectrumWidth*percent - spectrumWidth/2
    , labelY - spectrumHeight/2)
  where
    (nm,v) = maximumBy (comparing snd) dat
    labelY = v * spectrumHeight
    initNM = fromIntegral $ fst (head dat)
    lastNM = 700
    percent = (fromIntegral nm-initNM)/(lastNM-initNM)

drawLabel :: Text -> String -> Animation
drawLabel label c = animate $ const $
    translate (0) (-svgHeight labelSVG * 1.5) $
    withFillColor c $
    labelSVG
  where
    labelSVG =
      center $
      scale 1 $
      latex label

moveUp :: SVG -> SVG
moveUp svg = translate 0 (-svgHeight svg * 1.5) svg

moveDown :: SVG -> SVG
moveDown svg = translate 0 (svgHeight svg * 1.5) svg

interpolation :: Animation
interpolation = mkAnimation 2 $ \t ->
    mkGroup
    [ translate 0 3 $
      scaleXY (curveS 2 1) 0.2 $
      showColorMap $ interpolateRGB8 hsvComponents a b
    , translate 0 1 $
      scaleXY (curveS 2 1) 0.2 $
      showColorMap $ interpolateRGB8 xyzComponents a b
    , translate 0 (-1) $
      scaleXY (curveS 2 1) 0.2 $
      showColorMap $ interpolateRGB8 lchComponents a b
    , translate 0 (-3) $
      scaleXY (curveS 2 1) 0.2 $
      showColorMap $ interpolateRGB8 labComponents a b
    ]
  where
    a = cyan -- green
    b = red
    yellow = PixelRGB8 0xFF 0xFF 0x00
    green = PixelRGB8 0x00 0xFF 0x00
    blue = PixelRGB8 0x00 0x00 0xFF
    cyan = PixelRGB8 0x00 0xFF 0xFF
    red =  PixelRGB8 0xFF 0x00 0x00

lchColorSpace :: Int -> Int -> Tree
lchColorSpace width height = embedImage $ generateImage gen width height
  where
    toRad deg = deg/180 * pi
    gen x y =
      let
          h = fromToS 0 360 (fromIntegral x / fromIntegral width)
          aStar = (cos (toRad h) * c)
          bStar = (sin (toRad h) * c)
          l = findLStar aStar bStar
          c = fromToS 0 (sqrt (labScaleX^2 + labScaleY^2)) (fromIntegral y / fromIntegral height)
          -- RGB r g b = toSRGBBounded (colorPack lchComponents l c h)
          RGB r g b = toSRGBBounded (cieLAB d65 l aStar bStar)
      in PixelRGB8 r g b

hsvColorSpace :: Int -> Int -> Tree
hsvColorSpace width height = embedImage $ generateImage gen width height
  where
    toRad deg = deg/180 * pi
    gen x y =
      let
          h = fromToS 0 360 (fromIntegral x / fromIntegral width)
          v = 1
          s = fromToS 0 1 (fromIntegral y / fromIntegral height)
          RGB r g b = toSRGBBounded (colorPack hsvComponents h s v)
      in PixelRGB8 r g b

spacesA :: Animation
spacesA = mkAnimation 10 $ \t ->
  scaleToSize screenWidth screenHeight $
  if t < 0.3
    then cieLABImage 100 100
    else if t < 0.6
      then lchColorSpace 100 100
      else hsvColorSpace 100 100

highlightE :: Effect
highlightE d t =
  scale (1 + bellS 2 (t/d)*0.5) . rotate (wiggleS (t/d) * 20)

-- s-curve, sin, s-curve
wiggleS :: Signal
wiggleS t
  | t < 0.25  = curveS 2 (t*4)
  | t < 0.75  = sin ((t-0.25)*2*pi+pi/2)
  | otherwise = curveS 2 ((t-0.75)*4)-1

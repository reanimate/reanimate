{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo     #-}
module Spectrum
  ( scene2
  )
where

import           Control.Lens
import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad
import qualified Data.ByteString               as BS
import           Data.Colour
import           Data.Colour.CIE
import           Data.Colour.CIE.Illuminant
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV                 ( hsvView )
import           Data.Colour.SRGB
import           Data.Colour.SRGB.Linear
import           Data.List
import qualified Data.Map                      as Map
import           Data.Ord
import           Data.Text                                ( Text )
import           Graphics.SvgTree                  hiding ( Text )
import           Linear.V2
import           Reanimate
import           Reanimate.Builtin.CirclePlot
import           Reanimate.Builtin.Documentation
import qualified Reanimate.Builtin.TernaryPlot as Ternary
import           Reanimate.ColorComponents
import           Reanimate.ColorSpace
import           Reanimate.Scene
import           Reanimate.Raster
import           System.IO.Unsafe

import           Transcript

long = [ (nm, l) | (nm, (l, m, s)) <- Map.toList coneSensitivity ]
medium = [ (nm, m) | (nm, (l, m, s)) <- Map.toList coneSensitivity ]
short = [ (nm, s) | (nm, (l, m, s)) <- Map.toList coneSensitivity ]
xCoords = [ (nm, x / 2) | (nm, (x, y, z)) <- Map.toList bigXYZCoordinates ]
yCoords = [ (nm, y / 2) | (nm, (x, y, z)) <- Map.toList bigXYZCoordinates ]
zCoords = [ (nm, z / 2) | (nm, (x, y, z)) <- Map.toList bigXYZCoordinates ]

labScaleX = 110 -- 100 -- 128
labScaleY = 110 -- 100 -- 128

blueName = "royalblue"
greenName = "green"
redName = "maroon"

drawLabelS = drawLabelSVG "S" blueName
drawLabelM = drawLabelSVG "M" greenName
drawLabelL = drawLabelSVG "L" redName

drawLabelZ = drawLabelSVG "Z" blueName
drawLabelY = drawLabelSVG "Y" greenName
drawLabelX = drawLabelSVG "X" redName

illustrateSpectrum :: Scene s ()
illustrateSpectrum = spriteScope $ do
  intensity <- newSpriteSVG spectrumGridIntensity
  spriteE intensity $ overEnding 0.2 fadeOutE
  grid <- newSpriteSVG $ spectrumGrid False
  spriteZ grid 1
  waitUntil $ wordStart $ findWord ["theory"] "spectrum"
  forM_ (zip [0 ..] spectrum) $ \(nth, intensity) -> do
    let nm = fromIntegral nth * (300 / 23 / 2) + 400
    fork $ drawLine nm intensity
    wait 0.05
  waitUntil $ wordEnd $ findWord ["simplify"] "sensing"
 where
  spectrum =
    [0.02, 0.03, 0.07, 0.1, 0.2, 0.35, 0.7, 0.8, 0.9, 0.75, 0.5]
      ++ [0.3, 0.27, 0.31, 0.25, 0.3, 0.35, 0.4, 0.5]
      ++ [0.65, 0.8, 0.95, 1.0, 0.95, 0.98, 0.93, 0.97]
      ++ [0.85, 0.75, 0.65, 0.60, 0.55, 0.52, 0.48, 0.45]
      ++ [0.45, 0.40, 0.35, 0.30, 0.25, 0.20, 0.15]
      ++ [0.15, 0.15, 0.13, 0.11, 0.08]
  drawLine nm intensity = do
    let Just c = fmap (promotePixel . toRGB8) (nmToColor (round nm))
    s <-
      newSpriteSVG
      $ pathify
      $ withStrokeColorPixel c
      $ translate (-spectrumWidth / 2) (-spectrumHeight / 2)
      $ translate (fromToS 0 spectrumWidth ((nm - 400) / (700 - 400))) 0
      $ mkLine (0, 0) (0, spectrumHeight * intensity)
    spriteE s $ overEnding 0.3 fadeOutE
    spriteTween s 0.8 $ partialSvg . curveS 2

scene2 :: Scene s ()
scene2 = spriteScope $ do
  illustrateSpectrum
  oldGrid <- newSpriteSVG $ spectrumGrid False
  newGrid <- newSpriteSVG $ spectrumGrid True
  spriteZ newGrid 1
  spriteTween newGrid 0.5 withGroupOpacity
  destroySprite oldGrid

  (longL, longD) <- plotLineSprite newGrid redName
  writeVar longD long

  (mediumL, mediumD) <- plotLineSprite newGrid greenName
  writeVar mediumD medium

  (shortL, shortD) <- plotLineSprite newGrid blueName
  writeVar shortD short

  let drawCone keyVar svg dat key = fork $ do
        waitUntil $ wordStart $ findWord ["simplify"] key
        fork $ tweenVar keyVar 3 $ \v -> fromToS v 1
        label <- newSpriteSVG svg
        spriteE label $ overBeginning 0.3 fadeInE
        spriteE label $ overEnding 0.3 fadeOutE
        spriteMap label $ uncurry translate (labelPosition dat)
        waitUntil $ wordStart $ findWord ["xyz"] "stretched"
        destroySprite label

  drawCone shortL  drawLabelS short  "short"
  drawCone mediumL drawLabelM medium "medium"
  drawCone longL   drawLabelL long   "long"

  waitUntil $ wordStart $ findWord ["xyz"] "stretched"
  -- SML label are deleted now.

  waitOn $ do
    now <- queryNow
    let dur = wordEnd (findWord ["xyz"] "squeezed") - now
    fork $ tweenVar shortD dur $ \v -> morphSensitivity v zCoords . curveS 2
    fork $ tweenVar mediumD dur $ \v -> morphSensitivity v yCoords . curveS 2
    fork $ tweenVar longD dur $ \v -> morphSensitivity v xCoords . curveS 2

  -- XYZ labels and timings
  labelZ <- newSpriteSVG drawLabelZ
  spriteE labelZ $ overBeginning 0.3 fadeInE
  labelZPos <- spriteVar labelZ (labelPosition zCoords) $ uncurry translate

  labelY    <- newSpriteSVG drawLabelY
  spriteE labelY $ overBeginning 0.3 fadeInE
  labelYPos <- spriteVar labelY (labelPosition yCoords) $ uncurry translate

  labelX    <- newSpriteSVG drawLabelX
  spriteE labelX $ overBeginning 0.3 fadeInE
  labelXPos <- spriteVar labelX (labelPosition xCoords) $ uncurry translate

  waitUntil $ wordStart $ findWord ["xyz"] "2D"

  fork $ spriteTween newGrid 2 $ \t -> scale (fromToS 1 0.5 $ curveS 2 t)
  fork $ spriteTween newGrid 2 $ \t ->
    translate (fromToS 0 (screenWidth / 4) $ curveS 2 t) 0

  fork $ tweenVar labelZPos 2 $ \(x, y) t ->
    let (newX, newY) = Ternary.toOffsetCartesianCoords 0 0
        s            = curveS 2 t
    in  (fromToS x (newX * 5 - screenWidth / 4) s, fromToS y (newY * 5 - 1) s)

  fork $ tweenVar labelYPos 2 $ \(x, y) t ->
    let (newX, newY) = Ternary.toOffsetCartesianCoords 1 0
        s            = curveS 2 t
    in  (fromToS x (newX * 5 - screenWidth / 4) s, fromToS y (newY * 5) s)

  fork $ tweenVar labelXPos 2 $ \(x, y) t ->
    let (newX, newY) = Ternary.toOffsetCartesianCoords 0 1
        s            = curveS 2 t
    in  (fromToS x (newX * 5 - screenWidth / 4) s, fromToS y (newY * 5 - 1) s)

  wait 2

  redFactor   <- newVar 0
  greenFactor <- newVar 0
  blueFactor  <- newVar 0
  xyzSpace    <-
    newSprite
    $   cieXYImage
    <$> unVar redFactor
    <*> unVar greenFactor
    <*> unVar blueFactor
    <*> pure imgSize
  spriteMap xyzSpace $ translate (-screenWidth / 4) 0
  spriteZ xyzSpace (-1)

  waitUntil $ wordStart $ findWord ["pyramid"] "pyramid"
  spriteTween xyzSpace 0.5 $ aroundCenter . scale . curveS 2

  wait 1


  waitUntil $ wordStart $ findWord ["pyramid"] "reds"
  spriteE labelX (overBeginning 1 $ aroundCenterE highlightE)
  tweenVar redFactor 1 $ \v -> fromToS v 1 . curveS 2

  waitUntil $ wordStart $ findWord ["pyramid"] "greens"
  spriteE labelY (overBeginning 1 $ aroundCenterE highlightE)
  tweenVar greenFactor 1 $ \v -> fromToS v 1 . curveS 2

  waitUntil $ wordStart $ findWord ["pyramid"] "blues"
  spriteE labelZ (overBeginning 1 $ aroundCenterE highlightE)
  tweenVar blueFactor 1 $ \v -> fromToS v 1 . curveS 2

  waitUntil $ wordStart $ findWord ["pyramid"] "filter"

  let downShift = 1
  visCurve <- newVar 0
  waveLine <-
    fork $ newSpriteSVG $ translate (screenWidth / 4) 0 $ scale 0.5 $ translate
      0
      (-spectrumHeight / 2)
      wavelengthAxis
  spriteTween waveLine 1
    $ \d -> translate 0 (fromToS 0 (-downShift) $ curveS 2 d)
  -- wait 1
  visLine <-
    newSprite
    $   withStrokeWidth 0.03
    .   withStrokeColor "white"
    .   morphXYZCoordinates
    <$> unVar visCurve
  spriteZ visLine 1

  spriteTween waveLine 0.5 $ \t -> withGroupOpacity (1 - t)
  destroySprite waveLine

  visSide <- spriteVar visLine 0 $ \t ->
    translate 0 (-downShift)
      . translate (fromToS (screenWidth / 4) (-screenWidth / 4) $ curveS 2 t) 0
      . scale (fromToS 0.5 1 $ curveS 2 t)
      . translate 0 (fromToS (-spectrumHeight / 2) 0 $ curveS 2 t)
  fork $ tweenVar visSide 3 $ \v -> fromToS v 1 . curveS 2
  fork $ tweenVar visCurve 3 $ \v -> fromToS v 1 . curveS 2
  fork $ spriteTween visLine 3 $ \d ->
    translate 0 (fromToS 0 downShift $ curveS 2 d)

  obsVisible <- newVar 1
  gamut      <- newVar 0
  reorient   <- newVar 0
  cmOpacity  <- newVar 1
  cmDelta    <- newVar 0
  cmName     <- newVar "sinebow"
  cmFunc     <- newVar sinebow
  visSpace   <- fork $ newSprite $ do
    getObs      <- unVar obsVisible
    getGamut    <- unVar gamut
    getReorient <- unVar reorient
    getOpacity  <- unVar cmOpacity
    getDelta    <- unVar cmDelta
    getFunc     <- unVar cmFunc
    return $ translate (-screenWidth / 4) 0 $ mkGroup
      [ mkClipPath "visible" [simplify obsColors]
      , mkClipPath "sRGB"    [simplify $ sRGBTriangle getGamut]
      , withGroupOpacity getObs $ withClipPathRef (Ref "visible") $ mkGroup
        [cieXYImage 1 1 1 imgSize]
      , translate 0 (1 * getReorient)
      $ rotate (-gamutSlope sRGBGamut * getReorient)
      $ mkGroup
          [ scale (1 + 0.5 * getReorient)
          $ withClipPathRef (Ref "sRGB")
          $ mkGroup [cieXYImageGamut getGamut imgSize]
          , lowerTransformations
          $ scale (1 + 0.5 * getReorient)
          $ withGroupOpacity getOpacity
          $ cmToTernary getDelta getFunc
          ]
      , withGroupOpacity getReorient
      $ translate 0 3.5
      $ scale 0.5
      $ center
      $ withFillColor "white"
      $ latex "sRGB"
      ]

  colorMap <- newSprite $ do
    getDelta <- unVar cmDelta
    getFunc  <- unVar cmFunc
    getName  <- unVar cmName
    return $ mkGroup
      [ renderColorMap getDelta
                       (screenWidth * 0.75)
                       (screenHeight * 0.15)
                       getFunc
      , withGroupOpacity (getDelta * 2)
      $ withFillColor "white"
      $ translate 0 1.2
      $ scale 0.7
      $ center
      $ latex getName
      ]
  spriteMap colorMap $ translate 0 (-3)
  applyVar cmOpacity colorMap withGroupOpacity

  wait 4
  fork $ spriteTween visLine 1 $ \t -> withGroupOpacity (1 - t)
  fork $ spriteTween xyzSpace 1 $ \t -> withGroupOpacity (1 - t)
  wait 2

  fork $ spriteTween newGrid 0.5 $ \t -> withGroupOpacity (1 - t)

  fork $ tweenVar labelXPos 1 $ \(x, y) t ->
    let (newX, newY) = (-1, 3)
        s            = curveS 2 t
    in  (fromToS x newX s, fromToS y newY s)
  fork $ tweenVar labelYPos 1 $ \(x, y) t ->
    let (newX, newY) = (0, 3)
        s            = curveS 2 t
    in  (fromToS x newX s, fromToS y newY s)
  fork $ tweenVar labelZPos 1 $ \(x, y) t ->
    let (newX, newY) = (1, 3)
        s            = curveS 2 t
    in  (fromToS x newX s, fromToS y newY s)

  spriteZ visSpace (-1)

  spriteTween visSpace 1
    $ \t -> translate (fromToS 0 (screenWidth / 4) $ curveS 2 t) 0

  waitUntil $ wordStart $ findWord ["rgb"] "triangle"

  rgb <- newSprite $ withStrokeColor "white" . sRGBTriangle <$> unVar gamut
  spriteTween rgb 1 partialSvg

  waitUntil $ wordStart $ findWord ["rgb"] "Finally"
  fork $ spriteTween rgb 1 $ \t -> withGroupOpacity (1 - t)

  fork $ spriteTween labelX 1 $ \t -> withGroupOpacity (1 - t)
  fork $ spriteTween labelY 1 $ \t -> withGroupOpacity (1 - t)
  fork $ spriteTween labelZ 1 $ \t -> withGroupOpacity (1 - t)

  tweenVar obsVisible 1 $ \v -> fromToS v 0 . curveS 2

  tweenVar reorient 1 $ \v -> fromToS v 1 . curveS 2

  waitUntil $ wordStart $ findWord [] "path"

  writeVar cmName "jet"
  writeVar cmFunc jet
  tweenVar cmDelta 5 $ \d -> fromToS d 1

  waitUntil $ wordStart $ findWord ["hsv"] "Hue"

  spriteTween visSpace 1 $ \t -> translate (-2.5 * curveS 2 t) 0

  hsv <- newSprite $ do
    getOpacity <- unVar cmOpacity
    getDelta   <- unVar cmDelta
    getFunc    <- unVar cmFunc
    return $ mkGroup
      [ lowerTransformations $ scaleToWidth (screenWidth * 0.3) $ mkGroup
        [ hsvColorSpace 1000
        , withGroupOpacity getOpacity $ cmToHSV getDelta getFunc
        ]
      , translate 0 2 $ scale 0.5 $ center $ withFillColor "white" $ latex "HSV"
      ]

  spriteMap hsv $ translate 3 1.5
  spriteTween hsv 1 withGroupOpacity

  waitUntil $ wordStart $ findWord ["hsv"] "draw"

  let drawColormap generator txt = do
        tweenVar cmOpacity 0.3 $ \o t -> fromToS o 0 t
        writeVar cmName    txt
        writeVar cmFunc    generator
        writeVar cmOpacity 1
        writeVar cmDelta   0
        tweenVar cmDelta 5 $ \d -> fromToS d 1

  drawColormap sinebow "sinebow"

  waitUntil $ wordStart $ findWord ["lab"] "LAB"
  wait (-2)

  fork $ spriteTween hsv 1 $ \t -> translate (2 * curveS 2 t) 0
  spriteTween visSpace 1 $ \t -> translate (-2 * curveS 2 t) 0

  lab <- newSprite $ do
    getOpacity <- unVar cmOpacity
    getDelta   <- unVar cmDelta
    getFunc    <- unVar cmFunc
    return $ mkGroup
      [ lowerTransformations $ scaleToWidth (screenWidth * 0.20) $ mkGroup
        [ cieLABImagePixels
        , withGroupOpacity getOpacity $ cmToLAB getDelta getFunc
        ]
      , translate 0 2 $ scale 0.5 $ center $ withFillColor "white" $ latex "LAB"
      ]

  spriteMap lab $ translate 0 1.5
  spriteTween lab 1 withGroupOpacity

  waitUntil $ wordStart $ findWord ["made"] "Parula"
  drawColormap parula "parula"

  waitUntil $ wordStart $ findWord ["made"] "Viridis"
  drawColormap viridis "viridis"

  waitUntil $ wordStart $ findWord ["made"] "Cividis"
  drawColormap cividis "cividis"

  waitUntil $ wordStart $ findWord ["made"] "Turbo"
  drawColormap turbo "turbo"

  waitUntil $ wordEnd $ findWord ["grid"] "Looking"
 where
  imgSize   = 2000
  obsColors = lowerTransformations $ scale 5 renderXYZCoordinatesTernary


renderColorMap :: Double -> Double -> Double -> (Double -> PixelRGB8) -> SVG
renderColorMap delta width height cmap =
  translate (-width / 2 * (1 - delta)) 0 $ mkGroup
    [ scaleToSize (width * delta) height $ showColorMap (\t -> cmap (t * delta))
    , withStrokeWidth (defaultStrokeWidth * 0.7)
    $ withStrokeColor "white"
    $ withFillOpacity 0
    $ mkRect (width * delta) height
    ]

renderXYZCoordinatesTernary :: SVG
renderXYZCoordinatesTernary =
  withFillOpacity 0
    $ mkLinePath
    $ [ (x, y)
      | (_nm, (red, green, blue)) <- Map.toList lightXYZCoordinates
      , let (x, y) = Ternary.toOffsetCartesianCoords green red
      ]

morphXYZCoordinates :: Double -> SVG
morphXYZCoordinates t =
  withFillOpacity 0
    $ mkLinePath
    $ [ (fromToS linearX (x * 5) t, fromToS 0 (y * 5) t)
      | (nm, (red, green, blue)) <- Map.toList lightXYZCoordinates
      , let (x, y) = Ternary.toOffsetCartesianCoords green red
      , let percent = (fromIntegral nm - initNM) / (lastNM - initNM)
            linearX = percent * spectrumWidth - spectrumWidth / 2
      ]
 where
  dat    = Map.toList lightXYZCoordinates
  initNM = fromIntegral $ fst (head dat)
  lastNM = 700 -- fromIntegral $ fst (last dat)

-- Red corner: 0.64 0.33 0
-- Green corner: 0.3 0.6 0.1
-- Blue corner: 0.15 0.06 0.79
--1 0 0 -> 0.64 0.33 0.0
--0 1 0 -> 0.30 0.60 0.1
cieXYImageGamut :: Double -> Int -> SVG
cieXYImageGamut t density =
  cacheSvg ("cieXYImageGamut" :: String, t, density)
    $ Ternary.ternaryPlot density
    $ \aCoord bCoord cCoord ->
        let
          aCoord'   = fromToS aCoord (rX * aCoord + gX * bCoord + bX * cCoord) t
          bCoord'   = fromToS bCoord (rY * aCoord + gY * bCoord + bY * cCoord) t
          cCoord'   = fromToS cCoord (rZ * aCoord + gZ * bCoord + bZ * cCoord) t
          RGB r g b = toSRGBBounded (cieXYZ aCoord' bCoord' cCoord')
        in
          PixelRGBA8 r g b 0xFF
 where
  RGB r g b    = primaries sRGBGamut
  (rX, rY, rZ) = chromaCoords $ chromaConvert r
  (gX, gY, gZ) = chromaCoords $ chromaConvert g
  (bX, bY, bZ) = chromaCoords $ chromaConvert b

-- slope in degrees
gamutSlope :: RGBGamut -> Double
gamutSlope gamut = atan2 (y1 / y2) (x1 / x2) / pi * 180
 where
  RGB r g b     = primaries sRGBGamut
  (gX, gY, _gZ) = chromaCoords $ chromaConvert g
  (bX, bY, _bZ) = chromaCoords $ chromaConvert b
  (x1, y1)      = Ternary.toCartesianCoords gX gY
  (x2, y2)      = Ternary.toCartesianCoords bX bY

strokeLine :: Double -> [(Double, Double)] -> SVG
strokeLine t points = withFillOpacity 0 $ mkGroup
  [ withStrokeWidth (defaultStrokeWidth * 2)
  $  withStrokeColor "black"
  $  partialSvg t
  $  mkLinePath points
  &  strokeLineCap
  .~ pure CapRound
  , withStrokeWidth defaultStrokeWidth
  $  withStrokeColor "white"
  $  partialSvg t
  $  mkLinePath points
  &  strokeLineCap
  .~ pure CapRound
  ]

-- 0.15 0.06 -> 0 0
cmToTernary :: Double -> (Double -> PixelRGB8) -> SVG
cmToTernary 0 _  = mkGroup []
cmToTernary t cm = lowerTransformations $ scale 5 $ strokeLine t points
 where
  steps = 100
  points =
    [ Ternary.toOffsetCartesianCoords (cieY / s) (cieX / s)
    | n <- [0 .. steps]
    , let PixelRGB8 red green blue = cm (fromIntegral n / fromIntegral steps)
          (cieX, cieY, cieZ)       = cieXYZView (sRGB24 red green blue)
          s                        = cieX + cieY + cieZ
    ]

cmToHSV :: Double -> (Double -> PixelRGB8) -> SVG
cmToHSV t cm = lowerTransformations $ scale (screenHeight / 2) $ strokeLine
  t
  points
 where
  steps = 100
  points =
    [ (cos radian * s, sin radian * s)
    | n <- [0 .. steps]
    , let PixelRGB8 red green blue = cm (fromIntegral n / fromIntegral steps)
          (h, s, _v)               = hsvView (toSRGB $ sRGB24 red green blue)
          radian                   = h / 180 * pi
    ]

-- dim = 100
-- -labScaleX = 0
-- 0 = dim/2
-- +labScaleX = dim
-- -labScaleX to +labScaleX
cmToLAB :: Double -> (Double -> PixelRGB8) -> SVG
cmToLAB t cm =
  lowerTransformations
    $ translate (-screenHeight / 2) (-screenHeight / 2)
    $ strokeLine t points
 where
  steps = 100
  points =
    [ ( (a + labScaleX) / (labScaleX * 2) * screenHeight
      , (b + labScaleY) / (labScaleY * 2) * screenHeight
      )
    | n <- [0 .. steps]
    , let PixelRGB8 red green blue = cm (fromIntegral n / fromIntegral steps)
          (_l, a, b)               = cieLABView d65 (sRGB24 red green blue)
    ]


-- aCoord = red
-- bCoord = green
-- cCoord = blue
-- closer to white when (aCoord+cCoord) approaches 0
cieXYImage :: Double -> Double -> Double -> Int -> SVG
cieXYImage redFactor greenFactor blueFactor density =
  cacheSvg ("cieXYImage" :: String, redFactor, greenFactor, blueFactor, density)
    $ Ternary.ternaryPlot density
    $ \aCoord bCoord cCoord ->
        let white  = chromaColour d65 1
            c      = cieXYZ aCoord bCoord cCoord
            redD   = fromToS (1 - aCoord) 1 redFactor
            greenD = fromToS (1 - bCoord) 1 greenFactor
            blueD  = fromToS (1 - cCoord) 1 blueFactor
            RGB r g b =
                toSRGBBounded
                  $ blend (1 - redD)   white
                  $ blend (1 - greenD) white
                  $ blend (1 - blueD) white c
        in  promotePixel (PixelRGB8 r g b)

cieLABImagePixels :: SVG
cieLABImagePixels =
  scaleToHeight screenHeight $ embedImage $ unsafePerformIO $ do
    dat <- BS.readFile "lab.png"
    case decodePng dat of
      Left  err -> error err
      Right img -> return $ convertRGBA8 img

colorMapToLABCoords :: (Double -> PixelRGB8) -> SVG
colorMapToLABCoords colorMap = withFillOpacity 0 $ mkLinePath
  [ (aStar, 1 - bStar)
  | n <- [0 .. steps]
  , let PixelRGB8 r g b       = colorMap (n / steps)
        (lStar, aStar, bStar) = cieLABView d65 (sRGB24 r g b)
  ]
  where steps = 100

colorMapToXYZCoords :: (Double -> PixelRGB8) -> SVG
colorMapToXYZCoords colorMap = withFillOpacity 0 $ mkLinePath
  [ (x / s, 1 - (y / s))
  | n <- [0 .. steps]
  , let PixelRGB8 r g b = colorMap (n / steps)
        (x, y, z)       = cieXYZView (sRGB24 r g b)
        s               = x + y + z
  ]
  where steps = 100

sRGBTriangle :: Double -> SVG
sRGBTriangle t =
  lowerTransformations
    $ scale 5
    $ withFillOpacity 0
    $ withStrokeLineJoin JoinRound
    $ mkLinePathClosed
        [ Ternary.toOffsetCartesianCoords (fromToS rY 0 t) (fromToS rX 1 t)
        , Ternary.toOffsetCartesianCoords (fromToS gY 1 t) (fromToS gX 0 t)
        , Ternary.toOffsetCartesianCoords (fromToS bY 0 t) (fromToS bX 0 t)
        ]
 where
  RGB r g b   = primaries sRGBGamut
  (rX, rY, _) = chromaCoords $ chromaConvert r
  (gX, gY, _) = chromaCoords $ chromaConvert g
  (bX, bY, _) = chromaCoords $ chromaConvert b

spectrumHeight = screenHeight * 0.5
spectrumWidth = screenWidth * 0.7

wavelengthAxis :: SVG
wavelengthAxis = withStrokeWidth strokeWidth $ mkGroup
  [ translate (-spectrumWidth / 2) 0
    $  withFillOpacity 0
    $  withStrokeColor "white"
    $  mkPath
    $  [ MoveTo OriginAbsolute [V2 0 0]
       , VerticalTo OriginRelative [tickLength, -tickLength]
       , HorizontalTo OriginRelative [spectrumWidth]
       , VerticalTo OriginRelative [tickLength, -tickLength]
       ]
    ++ concat
         [ [ MoveTo OriginAbsolute [V2 (n / (nTicksX - 1) * spectrumWidth) 0]
           , VerticalTo OriginRelative [tickLength]
           ]
         | n <- [0 .. nTicksX - 1]
         ]
  ]
 where
  strokeWidth = 0.03
  nTicksX     = 24
  tickLength  = spectrumHeight * 0.02

spectrumGridIntensity :: SVG
spectrumGridIntensity = withFillColor "white"
  $ translate (-spectrumWidth * 0.5 - svgWidth svg * 0.7) 0 svg
  where svg = center $ scale 0.7 $ rotate 90 $ latex "Intensity $\\rightarrow$"

spectrumGrid :: Bool -> SVG
spectrumGrid includeSensitivity = withStrokeWidth strokeWidth $ mkGroup
  [ translate (-spectrumWidth / 2) (spectrumHeight / 2)
  $  withFillOpacity 0
  $  withStrokeColor "white"
  $  mkPath
  $  [ MoveTo OriginAbsolute [V2 0 0]
     , HorizontalTo OriginRelative [tickLength, -tickLength]
     , VerticalTo OriginRelative [-spectrumHeight]
     , HorizontalTo OriginRelative [spectrumWidth]
     , VerticalTo OriginRelative [tickLength, -tickLength]
     ]
  ++ concat
       [ [ MoveTo OriginAbsolute
                  [V2 (n / (nTicksX - 1) * spectrumWidth) (-spectrumHeight)]
         , VerticalTo OriginRelative [tickLength]
         ]
       | n <- [0 .. nTicksX - 1]
       ]
  ++ concat
       [ [ MoveTo OriginAbsolute
                  [V2 0 (n / (nTicksY - 1) * negate spectrumHeight)]
         , HorizontalTo OriginRelative [tickLength]
         ]
       | n <- [0 .. nTicksY - 1]
       ]
  , withFillColor "white" $ mkGroup
    [ translate (-spectrumWidth * 0.5 - svgWidth sensitivity * 0.7) 0
      $ if includeSensitivity then sensitivity else None
    , translate 0
                (-spectrumHeight * 0.5 - svgHeight wavelength * 0.7)
                wavelength
    , translate (-spectrumWidth * 0.5 + svgWidth shortWaves * 0.5)
                (-spectrumHeight * 0.5 - svgHeight shortWaves * 0.7)
                shortWaves
    , translate (spectrumWidth * 0.5 + svgWidth longWaves * 0.5)
                (-spectrumHeight * 0.5 - svgHeight longWaves * 0.7)
                longWaves
    ]
  ]
 where
  strokeWidth = 0.03
  nTicksX     = 24
  nTicksY     = fromIntegral (round (spectrumHeight / spectrumWidth * nTicksX))
  tickLength  = spectrumHeight * 0.02
  sensitivity =
    center $ scale 0.7 $ rotate 90 $ latex "Sensitivity $\\rightarrow$"
  wavelength = center $ scale 0.8 $ latex "Wavelength"
  shortWaves = rotate (-45) $ center $ scale 0.3 $ latex "400 nm"
  longWaves  = rotate (-45) $ center $ scale 0.3 $ latex "700 nm"

morphSensitivity
  :: [(Nanometer, Double)]
  -> [(Nanometer, Double)]
  -> Double
  -> [(Nanometer, Double)]
morphSensitivity datA datB m =
  [ (nm, fromToS a b m) | ((nm, a), (_, b)) <- zip datA datB ]

plotLineSprite
  :: Sprite s -> String -> Scene s (Var s Double, Var s [(Nanometer, Double)])
plotLineSprite parent color = do
  limitV <- newVar 0
  datV   <- newVar []
  spriteModify parent $ do
    l <- unVar limitV
    d <- unVar datV
    return $ \(svg, z) -> (mkGroup [sensitivitySVG l d color, svg], z)
  return (limitV, datV)

sensitivitySVG :: Double -> [(Nanometer, Double)] -> String -> SVG
sensitivitySVG limit dat c = mkGroup
  [ mkClipPath "spectrum"
    $ let margin = 1
      in  removeGroups
          $ simplify
          $ lowerTransformations
          $ translate 0 (margin / 2)
          $ pathify
          $ mkRect spectrumWidth (spectrumHeight + margin)
  , withClipPathRef (Ref "spectrum")
  $ simplify
  $ lowerTransformations
  $ withStrokeColor c
  $ withFillOpacity 0
  $ translate (-spectrumWidth / 2) (-spectrumHeight / 2)
  $ mkPath
  $ MoveTo OriginAbsolute [V2 0 0]
  : [ LineTo OriginAbsolute [V2 x y]
    | (nm, n) <- dat
    , fromIntegral nm <= lastNM
    , let percent = (fromIntegral nm - initNM) / (lastNM - initNM)
          x       = percent * spectrumWidth
          y       = n * spectrumHeight
    , percent <= limit
    ]
  ]
 where
  initNM = fromIntegral $ fst (head dat)
  lastNM = 700 -- fromIntegral $ fst (last dat)

labelPosition :: [(Nanometer, Double)] -> (Double, Double)
labelPosition dat =
  (spectrumWidth * percent - spectrumWidth / 2, labelY - spectrumHeight / 2)
 where
  (nm, v) = maximumBy (comparing snd) dat
  labelY  = v * spectrumHeight
  initNM  = fromIntegral $ fst (head dat)
  lastNM  = 700
  percent = (fromIntegral nm - initNM) / (lastNM - initNM)

drawLabelSVG :: Text -> String -> SVG
drawLabelSVG label c = translate 0 (svgHeight labelSVG * 0.7)
  $ withFillColor c labelSVG
  where labelSVG = center $ latex label

hsvColorSpace :: Int -> SVG
hsvColorSpace width =
  cacheSvg ("hsvColorSpace" :: String, width)
    $ circlePlot width
    $ \ang radius ->
        let h         = ang / pi * 180
            v         = 1
            s         = radius
            color     = colorPack hsvComponents h s v
            RGB r g b = toSRGBBounded color
        in  if inGamut sRGBGamut color
              then PixelRGBA8 r g b 0xFF
              else PixelRGBA8 0 0 0 0x00

highlightE :: Effect
highlightE d t =
  scale (1 + bellS 2 (t / d) * 0.5) . rotate (wiggleS (t / d) * 20)

-- s-curve, sin, s-curve
wiggleS :: Signal
wiggleS t | t < 0.25  = curveS 2 (t * 4)
          | t < 0.75  = sin ((t - 0.25) * 2 * pi + pi / 2)
          | otherwise = curveS 2 ((t - 0.75) * 4) - 1

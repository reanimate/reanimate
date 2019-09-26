#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Chiphunk.Low
import           Control.Lens
import           Control.Monad
import           Data.List
import Data.Ord
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text, pack)
import           Debug.Trace
import           Geom2D.CubicBezier  (ClosedPath (..), CubicBezier (..),
                                      PathJoin (..), bezierIntersection,
                                      bezierLineIntersections, closedPathCurves,
                                      evalBezier, rotateScaleVec, transform, Point(..))
import qualified Geom2D.CubicBezier  as G
import           Graphics.SvgTree    hiding (Text,Point)
import           Linear.V2
import           Numeric
import           Reanimate.Chiphunk
import           Reanimate.Constants
import           Reanimate.PolyShape
import           Reanimate.Driver    (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Monad
import           Reanimate.Signal
import           Reanimate.Svg
import           System.IO.Unsafe

test :: Animation
test = unsafePerformIO $ do
  bodyStore <- newBodyStore
  let gravity = Vect 0 (-1)

  -- Create an empty space.
  space <- spaceNew
  -- spaceCollisionSlop space $= (screenWidth/2560)
  spaceGravity space $= gravity

  -- Add a static line segment shape for the ground.
  -- We'll make it slightly tilted so the ball will roll off.
  -- We attach it to a static body to tell Chipmunk it shouldn't be movable.
  static <- get $ spaceStaticBody space
  ground <- segmentShapeNew static
    (Vect (-screenWidth/2) (screenHeight/2))
    (Vect (screenWidth/2) (-screenHeight/2)) 0
  shapeFriction ground $= 1
  spaceAddShape space ground

  -- Now let's make a ball that falls onto the line and rolls off.
  -- First we need to make a cpBody to hold the physical properties of the object.
  -- These include the mass, position, velocity, angle, etc. of the object.
  -- Then we attach collision shapes to the Body to give it a size and shape.

  let toVect (V2 x y) = Vect x y

  let svg = center $ scale 2 $ latex "$\\Phi$"
      poly = svgToPolyShapes svg
      vectGroup = plDecompose' 1 poly
            --mkCircle (Num radius)
      -- vects = svgToVects svg
      -- vects' = fst $ convexHull vects 0
      svg' =
        withFillOpacity 0 $ withStrokeColor "white" $
        withStrokeWidth (Num 0.01) $
        renderPolyShapes (map plFromPolygon vectGroup)
  --
  -- ballBody <- polyShapesToBody space poly
  -- bodyPosition ballBody $= Vect (-screenWidth/4) (screenHeight/2)
  --
  -- addToBodyStore bodyStore ballBody $
  --   withFillColor "white" $
  --   mkGroup
  --     [ svg' ]

  let splitPolys = vectGroup
  forM_ vectGroup $ \polygon -> do
    bd <- polygonsToBody space [map toVect polygon]
    bodyPosition bd $= Vect 0 (screenHeight/3)
    addToBodyStore bodyStore bd $
      renderPolyShape $ plFromPolygon polygon
      -- withFillColor "white" $
      -- mkGroup
      --   [ --withStrokeWidth (Num 0.005) $
      --     -- withStrokeWidth (Num 0.00) $
      --     -- withStrokeColor "white" $
      --     -- withFillOpacity 1 $
      --     renderPolyShape $ plFromPolygon polygon
      --   ]

  ani <- simulate space bodyStore 60 10 10
  spaceFreeRecursive space
  return ani

-- data LineCommand
--   = LineMove RPoint
--   -- | LineDraw RPoint
--   | LineBezier [RPoint]
--   | LineEnd

vectsToSVG :: [Vect] -> Tree
vectsToSVG (Vect x y:rest) =
  mkPath $
    MoveTo OriginAbsolute [V2 x y] :
    [ LineTo OriginAbsolute [V2 a b] | Vect a b <- rest ] ++
    [ EndPath ]
  where
    mkPath cmds = PathTree $ defaultSvg & pathDefinition .~ cmds

polygonsToSVG :: [[Vect]] -> Tree
polygonsToSVG = merge . mkGroup . map vectsToSVG
  where
    merge svg = PathTree $ defaultSvg & pathDefinition .~ extractPath svg

svgToVects :: Tree -> [Vect]
svgToVects svg = map worker (lineToPoints 200 cmds)
  where
    worker (V2 x y) = Vect x y
    cmds = toLineCommands $ wibble $ extractPath svg
    wibble xs = takeWhile (/=EndPath) xs ++ [EndPath]

chunkPolyshapes :: Tree -> Tree
chunkPolyshapes t =
  mkGroup
  [
  -- withFillColor "white" $
  -- withFillOpacity 1 $
  -- withStrokeWidth (Num 0.01) $
  -- withStrokeColor "blue" $
  -- mkGroup $ map renderPolyShape $
  -- map mergePolyShapeHoles $
  -- plGroupShapes $
  -- -- unionPolyShapes $
  -- traceUnion $
  -- svgToPolyShapes t
  -- , withStrokeWidth (Num 0.001) $
  --   withStrokeColor "red" $
  --   withFillOpacity 0 $
  --   mkGroup $ map renderPolyShape $
  --   svgToPolyShapes t
    withStrokeWidth (Num 0.001) $
    withStrokeColor "red" $
    mkGroup $ map renderPolyShape [pl12,pl13]
  ]

plArea :: PolyShape -> Double
plArea pl = areaForPoly (map toVect $ plPolygonify polyShapeTolerance pl) 0
  where
    toVect (Point x y) = Vect x y

reorient = scale 6 . translate 0 (-0.9)

traceUnion t =
  let out = unionPolyShapes t
  in trace (unlines $
            ("Trace: " ++ show (sum $ map plArea t)) :
            ("Check: " ++ show (sum $ map plArea out)) :
            map show t ++ ["Out:"] ++ map show out) out

pl1 = PolyShape {unPolyShape = ClosedPath [(Point 9.076152025049407e-2 0.8793158714390639,JoinLine),(Point 0.48865783997158957 0.5875446050720555,JoinLine),(Point 0.6291445682322502 0.560956966929786,JoinLine),(Point 0.684414274089106 0.6254399758525935,JoinLine),(Point 8.549729225029659e-2 0.9226054512958702,JoinLine)]}
pl2 = PolyShape {unPolyShape = ClosedPath [(Point 0.3174796674934045 (-0.17830385091116296),JoinLine),(Point 0.3930521201476444 (-0.22384314349780887),JoinLine),(Point 0.42400439387727545 (-0.17026635397131717),JoinLine),(Point 0.367180526197663 (-0.13743818486413273),JoinLine)]}
pl3 = PolyShape {unPolyShape = ClosedPath [(Point 3.818529643334073e-2 4.994390978944008e-2,JoinLine),(Point 3.5509598929490127e-3 3.0061273354143103e-2,JoinLine),(Point 2.6643683455205014e-2 (-1.3314171733262437e-2),JoinLine)]}
pl4 = PolyShape {unPolyShape = ClosedPath [(Point 8.041123459068245e-2 (-4.592522065745819e-2),JoinLine),(Point 0.29675416515553454 (-0.17235110729572378),JoinLine),(Point 0.34590008699238584 (-0.13081971694800998),JoinLine),(Point 0.22737727571852562 4.513304505856075e-2,JoinLine),(Point 0.10498823211784264 (-3.373201779345303e-3),JoinLine)]}
pl5 = PolyShape {unPolyShape = ClosedPath [(Point 4.1547772506933034e-2 0.10908491253751296,JoinLine),(Point 5.7556283592893165e-2 (-2.2936657573235042e-2),JoinLine),(Point 0.18645395693905423 2.887370173550552e-2,JoinLine),(Point 0.24063435970720265 0.12675372820097613,JoinLine),(Point 0.19807899284556882 0.17285980812856805,JoinLine),(Point 8.45668830191807e-2 0.18680141998908184,JoinLine)]}
pl6 = PolyShape {unPolyShape = ClosedPath [(Point (-0.2574090456009813) 0.22352645168090446,JoinLine),(Point (-0.27139525022076605) 0.16073053060077958,JoinLine),(Point (-9.444562231853837e-2) 4.994785063168361e-2,JoinLine),(Point (-4.012849544398306e-2) 6.733655689772311e-2,JoinLine),(Point (-4.6550067413171026e-2) 0.20017002713448606,JoinLine)]}
pl7 = PolyShape {unPolyShape = ClosedPath [(Point (-0.3714373753139388) 0.20874389306710772,JoinLine),(Point (-0.29452803703157854) 0.1655072120141602,JoinLine),(Point (-0.2835217605861194) 0.22889335799870397,JoinLine),(Point (-0.34012040364470725) 0.2621083279509192,JoinLine)]}
pl8 = PolyShape {unPolyShape = ClosedPath [(Point 0.1712163207191154 0.2149766965620984,JoinLine),(Point 0.19616659305280632 0.1905738263277067,JoinLine),(Point 0.6315796735654418 0.5328570813993471,JoinLine),(Point 0.49895896036053317 0.5862908698518825,JoinLine)]}
pl9 = PolyShape {unPolyShape = ClosedPath [(Point 0.10739670009207455 0.19390054402859663,JoinLine),(Point 0.16776933204268935 0.17681618959615353,JoinLine),(Point 0.19166035517967667 0.1970425575035583,JoinLine),(Point 0.16584071117957827 0.2205236594561943,JoinLine)]}
pl10 = PolyShape {unPolyShape = ClosedPath [(Point (-0.3669271639958152) 1.1382616686769857,JoinLine),(Point (-0.30169633405804164) 1.1454435521784956,JoinLine),(Point (-0.2858326377769962) 1.2078015601462844,JoinLine),(Point (-0.3736986541543818) 1.199765022618315,JoinLine)]}
pl11 = PolyShape {unPolyShape = ClosedPath [(Point (-3.0061173114158422e-2) 1.1779482589541495,JoinLine),(Point 5.4907529076574785e-3 1.2315286122957358,JoinLine),(Point (-3.4177530164954735e-2) 1.2269151735259667,JoinLine)]}
pl12 = PolyShape {unPolyShape = ClosedPath
  [(Point (-0.301730083226429*10) 1.1457103198223786,JoinLine)
  ,(Point (-0.10299641326632633*1) 1.0714677466302471,JoinLine)
  ,(Point (-3.162369257155477e-2*1) 1.182092647608354,JoinLine)
  --,(Point (-3.636877035247056e-2*1) 1.2310026399913667,JoinLine)
  --,(Point (-0.28589249688644447*1) 1.208074964264014,JoinLine)
  ]}
pl13 = PolyShape {unPolyShape = ClosedPath
  [(Point (-9.39646610882567e-2+2) 0.96018603098516,JoinLine)
  --,(Point (-6.465077411411442e-2*1) 0.9514465910530718,JoinLine)
  --,(Point (0.14134955357291654*1) 1.2461928296570437,JoinLine)
  ,(Point (8.29425965486209e-3) 1.2330682613363688,JoinLine)
  ,(Point (-0.10287959610529117) 1.0717052650151784,JoinLine)]}
pl14 = PolyShape {unPolyShape = ClosedPath [(Point (-0.6410657779553459) 0.5634659418454123,JoinLine),(Point (-0.641345175626591) 0.555247219163176,JoinLine),(Point (-0.4420444191709594) 0.5513358399243028,JoinLine),(Point (-0.10594072095664082) 0.9023816092645105,JoinLine),(Point (-7.577913627887163e-2) 0.9355404742795719,JoinLine),(Point (-0.10373629906690308) 0.9479533236181756,JoinLine)]}
pl15 = PolyShape {unPolyShape = ClosedPath [(Point (-3.523485505752828e-2) 0.1998408875682416,JoinLine),(Point 1.2546982880291841e-2 0.15208851089779252,JoinLine),(Point 0.13018455413761065 0.208301106923033,JoinLine),(Point 8.433578523127425e-2 0.9418697065712005,JoinLine),(Point (-7.884702019521485e-2) 0.932428903501462,JoinLine),(Point (-8.084970167352624e-2) 0.9296667902794287,JoinLine)]}
pl16 = PolyShape {unPolyShape = ClosedPath [(Point (-7.878197627394322e-2) 0.9319709148544943,JoinLine),(Point 8.438182768423004e-2 0.941734614325483,JoinLine),(Point 8.518191684165782e-2 0.9853357574063363,JoinLine),(Point 8.15663333974107e-2 0.9853939259545145,JoinLine),(Point (-4.936159754845773e-2) 0.9727171460725276,JoinLine)]}
pl17 = PolyShape {unPolyShape = ClosedPath [(Point (-4.99911351869741e-2) 0.9726181699897635,JoinLine),(Point 8.094809532371453e-2 0.9851776987711782,JoinLine),(Point 7.384845354621125e-2 1.0968271981504014,JoinLine),(Point 4.400324523877765e-2 1.1025518925313644,JoinLine)]}
pl18 = PolyShape {unPolyShape = ClosedPath [(Point 4.8482888984171635e-2 1.1105768787999895,JoinLine),(Point 7.512899062784217e-2 1.0959654499901599,JoinLine),(Point 0.2822087302816073 1.1420633097553252,JoinLine),(Point 0.27513287205836434 1.2060076125068004,JoinLine),(Point 0.20204313118994668 1.2231219927110881,JoinLine)]}
pl19 = PolyShape {unPolyShape = ClosedPath [(Point 0.2748784478150325 1.2078902086770202,JoinLine),(Point 0.2820822061918643 1.1439601884266248,JoinLine),(Point 0.3457225261391361 1.127943000720636,JoinLine),(Point 0.36082444597621127 1.187946730956635,JoinLine)]}
pl20 = PolyShape {unPolyShape = ClosedPath [(Point (-0.6864531908080556) 0.5032767356555317,JoinLine),(Point (-9.929294916146593e-2) 0.20829491627795255,JoinLine),(Point (-8.483230342755331e-2) 0.23523598173633797,JoinLine),(Point (-0.49217693932265094) 0.547917843618004,JoinLine)]}
pl21 = PolyShape {unPolyShape = ClosedPath [(Point (-9.274301086931341e-2) 0.2153671046631685,JoinLine),(Point (-4.68781647762479e-2) 0.2005165600411279,JoinLine),(Point (-4.790067311086227e-2) 0.23865704773924823,JoinLine),(Point (-7.795810209993242e-2) 0.2330461435659677,JoinLine)]}

unionTest = parsed
  where
    svg = center $ scale 2 $ latex "I"
    poly = svgToPolyShapes svg
    vectGroup = plDecompose' 1 poly
    rendered = mkGroup $ take 100 $ map (renderPolyShape . plFromPolygon) vectGroup
    parsed = svgToPolyShapes rendered

    myDecompose tol =
      concatMap decomposePolygon .
      map (plPolygonify tol) .
      map mergePolyShapeHoles .
      plGroupShapes .
      unionPolyShapes

main :: IO ()
main = reanimate $ bg `sim` mapA reorient (line `sim` mapA chunkPolyshapes test)
  where
    bg = mkAnimation 0 $ emit $ mkBackground "black"
    line = mkAnimation 0 $ emit $ withStrokeColor "white" $
      withStrokeWidth (Num 0.01) $
      mkLine (Num (-screenWidth/2), Num $ (screenHeight/2))
             (Num (screenWidth/2), Num $ -screenHeight/2)

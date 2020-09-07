#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Control.Lens                    ((^.))
import           Control.Monad.State
import qualified Data.Vector.Unboxed             as V
import           Graphics.SvgTree
import           Linear.Metric
import           Linear.V2                       (V2 (..))
import           Linear.Vector
import           Reanimate
import           Reanimate.Builtin.Documentation
import           Geom2D.CubicBezier.Linear

piSvg :: SVG
piSvg = pathify $ lowerTransformations $ center $ scale 10 $ latex "s"

main :: IO ()
main = reanimate $ docEnv $ mkAnimation 30 $ \t ->
  mkGroup
  [ mkBackgroundPixel rtfdBackgroundColor
  , piSvg
  , drawTangent t piSvg ]

drawTangent :: Double -> SVG -> SVG
drawTangent alpha | alpha >= 1 = id
drawTangent alpha = mapTree worker
  where
    worker (PathTree path) =
      let (V2 posX posY, tangent) =
            atPartial alpha $ toLineCommands $ path^.pathDefinition
          normed@(V2 tangentX tangentY) = normalize tangent ^* 4
          V2 midX midY = lerp 0.5 0 normed
          V2 normVectX normVectY = normalize tangent ^* (svgWidth normalTxt*1.1)
          tangentSvg =
            translate (posX) (posY) $
            rotate (unangle normed/pi*180 + 180) $
            translate 0 (svgHeight tangentTxt/2) $
            tangentTxt
          normalSvg =
            translate (posX) (posY) $
            rotate (unangle normed/pi*180 + 90) $
            translate (svgWidth normalTxt/2*1.1) (svgHeight normalTxt/2*1.3) $
            normalTxt
      in mkGroup
      [ withStrokeWidth (defaultStrokeWidth) $
        withStrokeColor "black" $
        translate (posX-midX) (posY-midY) $
        mkLine (0, 0) (tangentX, tangentY)
      , withStrokeWidth (defaultStrokeWidth) $
        withStrokeColor "black" $
        translate (posX) (posY) $
        mkLine (0, 0) (-normVectY, normVectX)
      , withStrokeWidth (defaultStrokeWidth*2) $
        withStrokeColor "white" $
        tangentSvg
      , withFillOpacity 1 $ withFillColor "black" $ withStrokeWidth 0 $
        tangentSvg
      , withStrokeWidth (defaultStrokeWidth*2) $
        withStrokeColor "white" $
        normalSvg
      , withFillOpacity 1 $ withFillColor "black" $ withStrokeWidth 0 $
        normalSvg
      ]
    worker t = t
    tangentTxt = scale 1.1 $ center $ latex "tangent"
    normalTxt = scale 1.1 $ center $ latex "normal"

atPartial :: Double -> [LineCommand] -> (V2 Double, V2 Double)
atPartial alpha cmds = evalState (worker 0 cmds) zero
  where
    worker _d [] = pure (0, 0)
    worker d (cmd:xs) = do
      from <- get
      len <- lineLength cmd
      let frac = (targetLen-d) / len
      if len == 0 || frac >= 1
        then worker (d+len) xs
        else do
          let bezier = lineCommandToBezier from cmd
              (pos, tangent) = evalBezierDeriv bezier frac
          pure $ (pos, tangent)
    totalLen = evalState (sum <$> mapM lineLength cmds) zero
    targetLen = totalLen * alpha

lineCommandToBezier :: V2 Coord -> LineCommand -> AnyBezier Coord
lineCommandToBezier from line =
  case line of
    LineBezier [a] ->
      AnyBezier $ V.fromList [from, a]
    LineBezier [a,b] ->
      AnyBezier $ V.fromList [from, a, b]
    LineBezier [a,b,c] ->
      AnyBezier $ V.fromList [from, a, b, c]
    _ -> error (show line)

unangle :: (Floating a, Ord a) => V2 a -> a
unangle a@(V2 ax ay) =
  let alpha = asin $ ay / norm a
  in if ax < 0
       then pi - alpha
       else alpha

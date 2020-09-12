-- |
-- Copyright   : Written by David Himmelstrup
-- License     : Unlicense
-- Maintainer  : lemmih@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Reanimate.Svg.LineCommand
  ( CmdM,
    LineCommand (..),
    lineLength,
    toLineCommands,
    lineToPath,
    lineToPoints,
    partialSvg,
  )
where

import Control.Lens ((%~), (&), (.~))
import Control.Monad.Fix
import Control.Monad.State
import Data.Functor
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified Geom2D.CubicBezier.Linear as Bezier
import Graphics.SvgTree
import Linear.Metric
import Linear.V2 hiding (angle)
import Linear.Vector

-- | Line command monad used for keeping track of the current location.
type CmdM a = State RPoint a

-- | Simplified version of a PathCommand where all points are absolute.
data LineCommand
  = LineMove RPoint
  | -- | LineDraw RPoint
    LineBezier [RPoint]
  | LineEnd RPoint
  deriving (Show)

-- | Convert from line commands to path commands.
lineToPath :: [LineCommand] -> [PathCommand]
lineToPath = map worker
  where
    worker (LineMove p) = MoveTo OriginAbsolute [p]
    -- worker (LineDraw p)         = LineTo OriginAbsolute [p]
    worker (LineBezier [a, b, c]) = CurveTo OriginAbsolute [(a, b, c)]
    worker (LineBezier [a, b]) = QuadraticBezier OriginAbsolute [(a, b)]
    worker (LineBezier [a]) = LineTo OriginAbsolute [a]
    worker LineBezier {} = error "Reanimate.Svg.lineToPath: invalid bezier curve"
    worker LineEnd {} = EndPath

-- | Using @n@ control points, approximate the path of the curves.
lineToPoints :: Int -> [LineCommand] -> [RPoint]
lineToPoints nPoints cmds =
  mapMaybe lineEnd lineSegments
  where
    lineSegments = [partialLine (fromIntegral n / fromIntegral nPoints) cmds | n <- [0 .. nPoints -1]]
    lineEnd [] = Nothing
    lineEnd [LineBezier pts] = Just (last pts)
    lineEnd (_ : xs) = lineEnd xs

partialLine :: Double -> [LineCommand] -> [LineCommand]
partialLine alpha cmds = evalState (worker 0 cmds) zero
  where
    worker _d [] = pure []
    worker d (cmd : xs) = do
      from <- get
      len <- lineLength cmd
      let frac = (targetLen - d) / len
      if len == 0 || frac >= 1
        then (cmd :) <$> worker (d + len) xs
        else pure [adjustLineLength frac from cmd]
    totalLen = evalState (sum <$> mapM lineLength cmds) zero
    targetLen = totalLen * alpha

adjustLineLength :: Double -> RPoint -> LineCommand -> LineCommand
adjustLineLength alpha from cmd =
  case cmd of
    LineBezier points -> LineBezier $ drop 1 $ partialBezierPoints (from : points) 0 alpha
    LineMove p -> LineMove p
    -- LineDraw t -> LineDraw (lerp alpha t from)
    LineEnd p -> LineBezier [lerp alpha p from]

-- | Estimated length of all segments in a line.
lineLength :: LineCommand -> CmdM Double
lineLength cmd =
  case cmd of
    LineMove to -> 0 <$ put to
    -- Straight line:
    LineBezier [dst] -> gets (distance dst) <* put dst
    -- Some kind of curve:
    LineBezier lst -> do
      from <- get
      let bezier = rpointsToBezier (from : lst)
          tol = 0.0001
      put (last lst)
      pure $ Bezier.arcLength bezier 1 tol
    LineEnd to -> gets (distance to) <* put to

rpointsToBezier :: [RPoint] -> Bezier.CubicBezier Double
rpointsToBezier lst =
  case lst of
    [a, b] -> Bezier.CubicBezier a a b b
    [a, b, c] -> Bezier.quadToCubic (Bezier.QuadBezier a b c)
    [a, b, c, d] -> Bezier.CubicBezier a b c d
    _ -> error $ "rpointsToBezier: Invalid list of points: " ++ show lst

-- | Convert from path commands to line commands.
toLineCommands :: [PathCommand] -> [LineCommand]
toLineCommands ps = evalState (worker zero Nothing ps) zero
  where
    worker _startPos _mbPrevControlPt [] = pure []
    worker startPos mbPrevControlPt (cmd : cmds) = do
      lcmds <- toLineCommand startPos mbPrevControlPt cmd
      let startPos' =
            case lcmds of
              [LineMove pos] -> pos
              _ -> startPos
      (lcmds ++) <$> worker startPos' (cmdToControlPoint $ last lcmds) cmds

cmdToControlPoint :: LineCommand -> Maybe RPoint
cmdToControlPoint (LineBezier points) = Just (last (init points))
cmdToControlPoint _ = Nothing

mkStraightLine :: RPoint -> LineCommand
mkStraightLine p = LineBezier [p]

toLineCommand :: RPoint -> Maybe RPoint -> PathCommand -> CmdM [LineCommand]
toLineCommand startPos mbPrevControlPt cmd =
  case cmd of
    MoveTo OriginAbsolute [] -> pure []
    MoveTo OriginAbsolute lst -> put (last lst) *> gets (pure . LineMove)
    MoveTo OriginRelative lst -> modify (+ sum lst) *> gets (pure . LineMove)
    LineTo OriginAbsolute lst -> forM lst (\to -> put to $> mkStraightLine to)
    LineTo OriginRelative lst -> forM lst (\to -> modify (+ to) *> gets mkStraightLine)
    HorizontalTo OriginAbsolute lst ->
      forM lst $ \x -> modify (_x .~ x) *> gets mkStraightLine
    HorizontalTo OriginRelative lst ->
      forM lst $ \x -> modify (_x %~ (+ x)) *> gets mkStraightLine
    VerticalTo OriginAbsolute lst ->
      forM lst $ \y -> modify (_y .~ y) *> gets mkStraightLine
    VerticalTo OriginRelative lst ->
      forM lst $ \y -> modify (_y %~ (+ y)) *> gets mkStraightLine
    CurveTo OriginAbsolute quads ->
      forM quads $ \(a, b, c) -> put c $> LineBezier [a, b, c]
    CurveTo OriginRelative quads ->
      forM quads $ \(a, b, c) -> do
        from <- get <* modify (+ c)
        pure $ LineBezier $ map (+ from) [a, b, c]
    SmoothCurveTo o lst -> mfix $ \result -> do
      let ctrl = mbPrevControlPt : map cmdToControlPoint result
      forM (zip lst ctrl) $ \((c2, to), mbControl) -> do
        from <- get <* adjustPosition o to
        let c1 = maybe (makeAbsolute o from c2) (mirrorPoint from) mbControl
        pure $ LineBezier [c1, makeAbsolute o from c2, makeAbsolute o from to]
    QuadraticBezier OriginAbsolute pairs ->
      forM pairs $ \(a, b) -> put b $> LineBezier [a, b]
    QuadraticBezier OriginRelative pairs ->
      forM pairs $ \(a, b) -> do
        from <- get <* modify (+ b)
        pure $ LineBezier $ map (+ from) [a, b]
    SmoothQuadraticBezierCurveTo o lst -> mfix $ \result -> do
      let ctrl = mbPrevControlPt : map cmdToControlPoint result
      forM (zip lst ctrl) $ \(to, mbControl) -> do
        from <- get <* adjustPosition o to
        let c1 = maybe from (mirrorPoint from) mbControl
        pure $ LineBezier [c1, makeAbsolute o from to]
    EllipticalArc o points ->
      concat
        <$> forM
          points
          ( \(rotX, rotY, angle, largeArc, sweepFlag, to) -> do
              from <- get <* adjustPosition o to
              return $ convertSvgArc from rotX rotY angle largeArc sweepFlag (makeAbsolute o from to)
          )
    EndPath -> put startPos $> [LineEnd startPos]
  where
    mirrorPoint c p = c * 2 - p
    adjustPosition OriginRelative p = modify (+ p)
    adjustPosition OriginAbsolute p = put p
    makeAbsolute OriginAbsolute _from p = p
    makeAbsolute OriginRelative from p = from + p

calculateVectorAngle :: Double -> Double -> Double -> Double -> Double
calculateVectorAngle ux uy vx vy
  | tb >= ta =
    tb - ta
  | otherwise =
    pi * 2 - (ta - tb)
  where
    ta = atan2 uy ux
    tb = atan2 vy vx

-- ported from: https://github.com/vvvv/SVG/blob/master/Source/Paths/SvgArcSegment.cs
{- HLINT ignore convertSvgArc -}
convertSvgArc :: RPoint -> Coord -> Coord -> Coord -> Bool -> Bool -> RPoint -> [LineCommand]
convertSvgArc (V2 x0 y0) radiusX radiusY angle largeArcFlag sweepFlag (V2 x y)
  | x0 == x && y0 == y =
    []
  | radiusX == 0.0 && radiusY == 0.0 =
    [LineBezier [V2 x y]]
  | otherwise =
    calcSegments x0 y0 theta1' segments'
  where
    sinPhi = sin (angle * pi / 180)
    cosPhi = cos (angle * pi / 180)

    x1dash = cosPhi * (x0 - x) / 2.0 + sinPhi * (y0 - y) / 2.0
    y1dash = - sinPhi * (x0 - x) / 2.0 + cosPhi * (y0 - y) / 2.0

    numerator = radiusX * radiusX * radiusY * radiusY - radiusX * radiusX * y1dash * y1dash - radiusY * radiusY * x1dash * x1dash

    s = sqrt (1.0 - numerator / (radiusX * radiusX * radiusY * radiusY))
    rx = if (numerator < 0.0) then (radiusX * s) else radiusX
    ry = if (numerator < 0.0) then (radiusY * s) else radiusY
    root =
      if (numerator < 0.0)
        then (0.0)
        else
          ( (if ((largeArcFlag && sweepFlag) || (not largeArcFlag && not sweepFlag)) then (-1.0) else 1.0)
              * sqrt (numerator / (radiusX * radiusX * y1dash * y1dash + radiusY * radiusY * x1dash * x1dash))
          )

    cxdash = root * rx * y1dash / ry
    cydash = - root * ry * x1dash / rx

    cx = cosPhi * cxdash - sinPhi * cydash + (x0 + x) / 2.0
    cy = sinPhi * cxdash + cosPhi * cydash + (y0 + y) / 2.0

    theta1' = calculateVectorAngle 1.0 0.0 ((x1dash - cxdash) / rx) ((y1dash - cydash) / ry)
    dtheta' = calculateVectorAngle ((x1dash - cxdash) / rx) ((y1dash - cydash) / ry) ((- x1dash - cxdash) / rx) ((- y1dash - cydash) / ry)
    dtheta =
      if (not sweepFlag && dtheta' > 0)
        then (dtheta' - 2 * pi)
        else (if (sweepFlag && dtheta' < 0) then dtheta' + 2 * pi else dtheta')

    segments' = ceiling (abs (dtheta / (pi / 2.0)))
    delta = dtheta / fromInteger segments'
    t = 8.0 / 3.0 * sin (delta / 4.0) * sin (delta / 4.0) / sin (delta / 2.0)

    calcSegments startX startY theta1 segments
      | segments == 0 =
        []
      | otherwise =
        LineBezier
          [ V2 (startX + dx1) (startY + dy1),
            V2 (endpointX + dxe) (endpointY + dye),
            V2 endpointX endpointY
          ] :
        calcSegments endpointX endpointY theta2 (segments - 1)
      where
        cosTheta1 = cos theta1
        sinTheta1 = sin theta1
        theta2 = theta1 + delta
        cosTheta2 = cos theta2
        sinTheta2 = sin theta2

        endpointX = cosPhi * rx * cosTheta2 - sinPhi * ry * sinTheta2 + cx
        endpointY = sinPhi * rx * cosTheta2 + cosPhi * ry * sinTheta2 + cy

        dx1 = t * (- cosPhi * rx * sinTheta1 - sinPhi * ry * cosTheta1)
        dy1 = t * (- sinPhi * rx * sinTheta1 + cosPhi * ry * cosTheta1)

        dxe = t * (cosPhi * rx * sinTheta2 + sinPhi * ry * cosTheta2)
        dye = t * (sinPhi * rx * sinTheta2 - cosPhi * ry * cosTheta2)

partialBezierPoints :: [RPoint] -> Double -> Double -> [RPoint]
partialBezierPoints ps a b =
  let c1 = Bezier.AnyBezier (V.fromList ps)
      Bezier.AnyBezier os = Bezier.bezierSubsegment c1 a b
   in V.toList os

-- | Create an image showing portion of a path.
--     Note that this only affects paths (see 'Reanimate.Svg.Constructors.mkPath').
--     You can also use this with other SVG shapes if you convert them to path first (see 'Reanimate.Svg.pathify').
--
--     Typical usage:
--
--    > animate $ \t -> partialSvg t myPath
partialSvg ::
  -- | number between 0 and 1 inclusively, determining what portion of the path to show
  Double ->
  -- | Image representing a path, of which we only want to display a portion determined by the first argument
  Tree ->
  Tree
partialSvg alpha | alpha >= 1 = id
partialSvg alpha = mapTree worker
  where
    worker (PathTree path) =
      PathTree $ path & pathDefinition %~ lineToPath . partialLine alpha . toLineCommands
    worker t = t

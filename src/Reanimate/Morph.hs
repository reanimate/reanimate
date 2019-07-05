{-# LANGUAGE ParallelListComp #-}
module Reanimate.Morph where

import           Control.Lens
import           Control.Monad.State
import           Linear.Metric
import           Linear.V2
import Data.List
import Data.Ord
import           Linear.Vector
import qualified Geom2D.CubicBezier          as Bezier
import           Graphics.SvgTree

import Reanimate.Svg
import Reanimate.Monad
import Reanimate.LaTeX

import Debug.Trace

{-
alignCommands :: [LineCommand] -> [LineCommand] -> ([LineCommand], [LineCommand])
alignCommands a b =
    (worker (glyphs [] a) (glyphs [] b)
    ,worker (glyphs [] b) (glyphs [] a))
  where
    glyphs acc [] = [reverse acc]
    glyphs [] (LineMove p:cs) = glyphs [LineMove p] cs
    glyphs acc (LineMove p:cs) = reverse acc : glyphs [LineMove p] cs
    glyphs acc (c:cs) = glyphs (c:acc) cs

    worker :: [[LineCommand]] -> [[LineCommand]] -> [LineCommand]
    worker [] [] = []
    worker xs [] = concat xs
    worker [] ((LineMove p:ps):xs) = LineMove p : [LineBezier [p,p,p] | _ <- ps]
    worker (x:xs) (y:ys) = align x y ++ worker xs ys

    align :: [LineCommand] -> [LineCommand] -> [LineCommand]
    align x y = addControlPoints (length y - length x) x

addControlPoints :: Int -> [LineCommand] -> [LineCommand]
addControlPoints n cmds | n <= 0 = cmds
addControlPoints n cmds = evalState (worker 0 cmds) zero
  where
    worker d [] = pure []
    worker d (cmd:xs) = do
      from <- get
      len <- lineLength cmd
      if d+len > pointDistance
        then do
          put from
          let newLen = pointDistance-d
              (cmdBefore, cmdAfter) = splitLineLength (newLen / len) from cmd
          _ <- lineLength cmdBefore
          (cmdBefore:) <$> worker 0 (cmdAfter : xs)
        else (cmd:) <$> worker (d+len) xs
    totalLen = evalState (sum <$> mapM lineLength cmds) zero
    pointDistance = totalLen / fromIntegral (n+1)

splitLineLength :: Double -> RPoint -> LineCommand -> (LineCommand, LineCommand)
splitLineLength alpha from cmd =
  case cmd of
    LineBezier points ->
      (LineBezier $ drop 1 $ partial_bezier_points (from:points) 0 alpha
      ,LineBezier $ drop 1 $ partial_bezier_points (from:points) alpha 1)
    LineMove p -> (LineMove p, LineMove p)
    LineEnd -> (LineEnd, LineEnd)
-}
interpolateLineCommands :: Double -> [LineCommand] -> [LineCommand] -> [LineCommand]
interpolateLineCommands alpha x y = map worker (zip x y)
  where
    worker (LineMove p1, LineMove p2) = LineMove (lerp alpha p1 p2)
    worker (LineBezier ps1, LineBezier ps2) =
      LineBezier [lerp alpha x y | (x,y) <- merge ps1 ps2]
    worker (LineEnd, LineEnd) = LineEnd
    worker (x,y) = error (show (x,y))
    merge [] [] = []
    merge [x] [y] = [(x,y)]
    merge (x:xs) [y] = (x,y) : merge xs [y]
    merge [x] (y:ys) = (x,y) : merge [x] ys
    merge (x:xs) (y:ys) = (x,y) : merge xs ys


boxCommands :: [LineCommand]
boxCommands = toLineCommands $ extractPath $ center $ mkPathString
  "M10,5 v-5 h-10 v10 h10 z M5,8 h-3 v-6 h6 v6 z"

squareCommands :: [LineCommand]
squareCommands = toLineCommands $ extractPath $ center $ scale 4 $ mkPathString
  "M10,5 v-5 h-10 v10 h10 z"

oCommands :: [LineCommand]
oCommands = toLineCommands $ extractPath $ center $ scale 10 $ mkPathString
  "M4.692403,-2.132005 C4.692403,-3.407223 3.696139,-4.463263 2.49066,-4.463263 C1.24533,-4.463263 0.278954,-3.377335 0.278954,-2.132005 C0.278954,-0.846824 1.315068,0.109589 2.480697,0.109589 C3.686177,0.109589 4.692403,-0.86675 4.692403,-2.132005 L4.692403,-2.132005 z\
  \M2.49066,-0.139477 C2.062267,-0.139477 1.62391,-0.348692 1.354919,-0.806974 C1.105853,-1.24533 1.105853,-1.853051 1.105853,-2.211706 C1.105853,-2.600249 1.105853,-3.138232 1.344956,-3.576588 C1.613948,-4.034869 2.082192,-4.244085 2.480697,-4.244085 C2.919054,-4.244085 3.347447,-4.024907 3.606476,-3.596513 S3.865504,-2.590286 3.865504,-2.211706 C3.865504,-1.853051 3.865504,-1.315068 3.646326,-0.876712 C3.427148,-0.428394 2.988792,-0.139477 2.49066,-0.139477 L2.49066,-0.139477 z"

innerOCommands :: [LineCommand]
innerOCommands = toLineCommands $ extractPath $ center $ scale 10 $ mkPathString
  "M2.49066,-0.139477 C2.062267,-0.139477 1.62391,-0.348692 1.354919,-0.806974 C1.105853,-1.24533 1.105853,-1.853051 1.105853,-2.211706 C1.105853,-2.600249 1.105853,-3.138232 1.344956,-3.576588 C1.613948,-4.034869 2.082192,-4.244085 2.480697,-4.244085 C2.919054,-4.244085 3.347447,-4.024907 3.606476,-3.596513 S3.865504,-2.590286 3.865504,-2.211706 C3.865504,-1.853051 3.865504,-1.315068 3.646326,-0.876712 C3.427148,-0.428394 2.988792,-0.139477 2.49066,-0.139477 L2.49066,-0.139477 z"

{-
A figure consists of a set of objects.
Each object is either a string or a loop.
Each object has a non-negative integer rank.

Strings can be reversed.
Loops can be reversed and shifted left/right.
-}

-- Invariant: End point is the same as starting point
data Loop = Loop RPoint [[RPoint]]
  deriving (Show, Eq)

loopStart :: Loop -> RPoint
loopStart (Loop start _) = start

shiftRight :: Loop -> Loop
shiftRight (Loop start (x:xs)) = Loop (last x) (xs ++ [x])

reverseLoop :: Loop -> Loop
reverseLoop (Loop start ps) = Loop start (drop 1 $ worker [] (reverse $ map reverse ps))
  where
    worker rest [s:cs] = (rest++[s]) : [cs ++ [start]]
    worker rest ((s:cs):xs) = (rest ++ [s]) : worker cs xs

loopToCommands :: Loop -> [LineCommand]
loopToCommands (Loop start curves) =
  LineMove start :
  [ LineBezier bs | bs <- curves ] ++
  [LineEnd]

commandsToLoops :: [LineCommand] -> [Loop]
commandsToLoops (LineMove start : xs) = map simplifyLoop $ worker start [] xs
  where
    worker start acc [] = [Loop start (reverse acc)]
    worker start acc (LineMove newStart : xs) =
      Loop start (reverse acc) : worker newStart [] xs
    worker start acc (LineBezier cs : xs) =
      worker start (cs:acc) xs
    worker start acc (LineEnd : xs) =
      worker start acc xs

simplifyLoop :: Loop -> Loop
simplifyLoop (Loop start cs) = Loop start (worker start cs)
  where
    worker from ([x]:xs)
      | from == x = worker from xs
    worker from (curves:cs) =
      curves : worker (last curves) cs
    worker from [] = []

loopLength :: Loop -> Double
loopLength (Loop start curves) = worker start curves
  where
    worker :: RPoint -> [[RPoint]] -> Double
    worker from [] = 0
    worker from (bezier:cs) =
      Bezier.arcLength (mkBezier from bezier) 1 tolerance + worker (last bezier) cs

    tolerance = 0.1

mkBezier :: RPoint -> [RPoint] -> Bezier.CubicBezier Double
mkBezier a cs = case cs of
    [b]   -> Bezier.CubicBezier (mkPoint a) (mkPoint b) (mkPoint b) (mkPoint b)
    [b,c] -> Bezier.quadToCubic (Bezier.QuadBezier (mkPoint a) (mkPoint b) (mkPoint c))
    [b,c,d]   -> Bezier.CubicBezier (mkPoint a) (mkPoint b) (mkPoint c) (mkPoint d)
  where
    mkPoint (V2 a1 a2) = Bezier.Point a1 a2

fromBezier :: Bezier.CubicBezier Double -> [RPoint]
fromBezier (Bezier.CubicBezier _ b c d) =
    [fromPoint b,fromPoint c,fromPoint d]
  where
    fromPoint (Bezier.Point x y) = V2 x y

loopCtrlPoints :: Loop -> [Double]
loopCtrlPoints (Loop start curves) = worker start 0 curves
  where
    worker from d [] = []
    worker from d (bezier:cs) =
      let d' = d + Bezier.arcLength (mkBezier from bezier) 1 0.1 in
      d' / totalLen : worker (last bezier) d' cs
    totalLen = loopLength (Loop start curves)

addCtrlPoints :: Loop -> [Double] -> Loop
addCtrlPoints (Loop start cs) ctrlPoints = Loop start (worker start 0 cs ctrlPoints)
  where
    totalLen = loopLength (Loop start cs)
    worker from d [] _ = []
    worker from d cs [] = cs
    worker from d (curves:cs) (p:ps) =
      let bezier = mkBezier from curves
          len = Bezier.arcLength bezier 1 0.1 in
      if d+len > p*totalLen
        then
          let newLen = p*totalLen - d
              param = Bezier.arcLengthParam bezier newLen 0.1
              (before,after) = Bezier.splitBezier bezier param
              beforeLst = fromBezier before
          in beforeLst : worker (last beforeLst) (d+newLen) (fromBezier after : cs) ps
        else curves: worker (last curves) (d+len) cs (p:ps)

setLoopStart :: Loop -> Double -> Loop
setLoopStart loop newStart =
    worker (loopCtrlPoints loop') loop'
  where
    loop' = addCtrlPoints loop [newStart]
    worker [] l = l
    worker (x:xs) l
      | x < newStart = worker xs (shiftRight l)
      | otherwise = l


closestPoint :: Loop -> RPoint -> (RPoint, Double)
closestPoint (Loop start cs) p = worker start 0 0 start cs
  where
    pPoint = mkPoint p
    mkPoint (V2 a1 a2) = Bezier.Point a1 a2
    fromPoint (Bezier.Point x y) = V2 x y
    worker :: RPoint -> Double -> Double -> RPoint -> [[RPoint]] -> (RPoint, Double)
    worker point at pos _ [] = (point, at / loopLength (Loop start cs))
    worker point at pos from (curves:cs) =
      let bezier = mkBezier from curves
          c = Bezier.closest bezier pPoint 0.1
          len = Bezier.arcLength bezier c 0.1
          totalLen = Bezier.arcLength bezier 1 0.1
          newPoint = fromPoint $ Bezier.evalBezier bezier c
      in if distance newPoint p < distance point p
        then worker newPoint (pos+len) (pos+totalLen) (last curves) cs
        else worker point at (pos+totalLen) (last curves) cs

loopDistance :: Loop -> Loop -> Double
loopDistance (Loop s1 c1) (Loop s2 c2) =
    distance s1 s2 + worker c1 c2
  where
    worker [] [] = 0
    worker (c1:cs1) (c2:cs2) =
      distance (last c1) (last c2) +
      worker cs1 cs2
    worker _ _ = error $ "Bad lengths: " ++ show (length c1, length c2)

loopBestAlign :: Loop -> Loop -> (Loop, Loop)
loopBestAlign orig foreign_ =
    if loopDistance orig' foreign'' < loopDistance origRev foreignRev
      then (orig', foreign'')
      else (origRev, foreignRev)
  where
    loopStart (Loop s _) = s
    (_, newStartDist) = closestPoint foreign_ (loopStart orig)
    foreign' = setLoopStart foreign_ newStartDist
    foreign'' = addCtrlPoints foreign' (init $ loopCtrlPoints orig)
    foreignRev = addCtrlPoints (reverseLoop foreign') (init $ loopCtrlPoints orig)
    orig' = addCtrlPoints orig (init $ loopCtrlPoints foreign')
    origRev = addCtrlPoints orig (init $ loopCtrlPoints (reverseLoop foreign'))

loopContains :: Loop -> Loop -> Bool
loopContains a (Loop b _) =
  odd (length $ loopIntersections a b)

loopIntersections_ :: Loop -> Loop -> [RPoint]
loopIntersections_ l1 (Loop start _) = loopIntersections l1 start

loopIntersections :: Loop -> RPoint -> [RPoint]
loopIntersections (Loop start cs) point = worker start cs
  where
    V2 pointX pointY = point
    mkPoint (V2 a1 a2) = Bezier.Point a1 a2
    fromPoint (Bezier.Point x y) = V2 x y
    farPoint = (Bezier.Point 1000 pointY) -- FIXME
    -- line = Bezier.Line (mkPoint point) farPoint
    lineBezier = Bezier.CubicBezier (mkPoint point) farPoint farPoint farPoint
    worker from [] = []
    worker from (curves : cs) =
      let bezier = mkBezier from curves
          -- inter = Bezier.bezierLineIntersections bezier line 0
          inter = map fst $ Bezier.bezierIntersection bezier lineBezier 0.00001
          pts = [ V2 x y | V2 x y <- map (fromPoint . Bezier.evalBezier bezier) inter ]
      in pts ++ worker (last curves) cs

data LoopTree = LoopBranch Loop LoopForest
  deriving (Show, Eq)
type LoopForest = [LoopTree]

singleton :: Loop -> LoopTree
singleton l = LoopBranch l []

insertTree :: LoopForest -> Loop -> LoopForest
insertTree [] l = [singleton l]
insertTree (LoopBranch b sub:xs) l
  | loopContains b l = LoopBranch b (insertTree sub l) : xs
  | loopContains l b = LoopBranch l [singleton b] : xs
  | otherwise        = LoopBranch b sub : insertTree xs l

forestFromList :: [Loop] -> LoopForest
forestFromList = foldl insertTree []

zeroLoop :: Loop -> Loop
zeroLoop (Loop start cs) = Loop start (map worker cs)
  where
    worker _bezier = [start]

zeroLoopAt :: Loop -> Maybe RPoint -> Loop
zeroLoopAt (Loop start cs) Nothing = Loop center (map (const [center]) cs)
  where
    center = loopCenter (Loop start cs)
zeroLoopAt (Loop _ cs) (Just start) = Loop start (map (const [start]) cs)

pairLoopForests :: LoopForest -> LoopForest -> [(Loop, Loop)]
pairLoopForests [] [] = []
pairLoopForests [] (LoopBranch b bSub:bs) =
    (zeroLoop b, b) : pairLoopForests [] bSub ++ pairLoopForests [] bs
pairLoopForests (LoopBranch a aSub:as) [] =
    (a, zeroLoop a) : pairLoopForests aSub [] ++ pairLoopForests as []
pairLoopForests (LoopBranch self selfSub:xs) bs =
    let (a, b, sub, rest) = findMatch self bs
    in (a,b) : pairLoopForests selfSub sub ++ pairLoopForests xs rest
  where
    findMatch :: Loop -> LoopForest -> (Loop, Loop, LoopForest, LoopForest)
    findMatch self (t@(LoopBranch l lSub) : xs) =
        let (a,b) = loopBestAlign self l
        in worker a b lSub (delete t bs) xs
      where
        worker a b lSub rest [] = (a,b,lSub, rest)
        worker a b lSub rest (t@(LoopBranch l lSub'): xs) =
          let (a', b') = loopBestAlign self l in
          if loopDistance a b < loopDistance a' b'
            then worker a b lSub rest xs
            else worker a' b' lSub' (delete t bs) xs

-- Assume loops are polygons
loopCenter :: Loop -> RPoint
loopCenter (Loop start cs) = V2 cx cy -- start --FIXME
  where
    pts = start : map last cs
    cx = sum [ (x1+x2)*(x1*y2 - x2*y1) | (V2 x1 y1,V2 x2 y2) <- zip pts (tail pts) ] /
          (6 * area)
    cy = sum [ (y1+y2)*(x1*y2 - x2*y1) | (V2 x1 y1,V2 x2 y2) <- zip pts (tail pts) ] /
          (6 * area)
    area = sum [ x1*y2 - x2*y1 | (V2 x1 y1,V2 x2 y2) <- zip pts (tail pts) ] / 2

permPair :: LoopForest -> LoopForest -> [(Loop,Loop)]
permPair a b =
    minimumBy (comparing loopDistances) $
    simplePair Nothing Nothing <$> permutations a <*> permutations b
  where
    loopDistances ls = sum [ loopDistance a b | (a,b) <- ls ]
    simplePair :: Maybe RPoint -> Maybe RPoint -> LoopForest -> LoopForest -> [(Loop,Loop)]
    simplePair srcCenter dstCenter a b =
      case (a,b) of
        ([],[]) -> []
        (LoopBranch x xSub:xs,[]) ->
          (x,zeroLoopAt x dstCenter) :
            simplePair srcCenter dstCenter xSub [] ++
            simplePair srcCenter dstCenter xs []
        ([], LoopBranch x xSub:xs) ->
          (zeroLoopAt x srcCenter,x) :
            simplePair srcCenter dstCenter [] xSub ++
            simplePair srcCenter dstCenter [] xs
        (LoopBranch al aSub:as, LoopBranch bl bSub:bs) ->
          let (aBest, bBest) = loopBestAlign al bl
              srcCenter' = Just $ loopCenter aBest
              dstCenter' = Just $ loopCenter bBest
          in
          (aBest, bBest) : simplePair srcCenter' dstCenter' aSub bSub ++
                           simplePair srcCenter dstCenter as bs

morph :: Tree -> Tree -> (Double -> Tree)
morph a b =
  let aF = forestFromList $ commandsToLoops $ toLineCommands $ extractPath a
      bF = forestFromList $ commandsToLoops $ toLineCommands $ extractPath b
      -- (aLoops, bLoops) = unzip $ pairLoopForests aF bF
      (aLoops, bLoops) = unzip $ permPair aF bF
      aCmds = concatMap loopToCommands aLoops
      bCmds = concatMap loopToCommands bLoops
  in \d -> PathTree $ defaultSvg & pathDefinition .~ lineToPath (interpolateLineCommands (1-d) aCmds bCmds)

annotatePath :: Tree -> Tree
annotatePath = mkGroup . reverse . map worker . toLineCommands . extractPath
  where
    mkCircle (V2 x y) = CircleTree $ defaultSvg
      & circleCenter .~ (Num x, Num y)
      & circleRadius .~ Num 1
    worker (LineMove p) = withFillColor "green" $ mkCircle p
    worker (LineBezier cs) = withFillColor "red" $ mkCircle (last cs)
    worker LineEnd = mkGroup []

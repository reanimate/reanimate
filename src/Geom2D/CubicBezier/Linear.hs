{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-|
Module      : Geom2D.CubicBezier.Linear
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX

Convenience wrapper around 'Geom2D.CubicBezier'

-}
module Geom2D.CubicBezier.Linear
  ( AnyBezier(..)
  , CubicBezier(..)
  , QuadBezier(..)
  , OpenPath(..)
  , ClosedPath(..)
  , PathJoin(..)
  , ClosedMetaPath(..)
  , OpenMetaPath(..)
  , MetaJoin(..)
  , MetaNodeType(..)
  , FillRule(..)
  , Tension(..)
  , quadToCubic
  , arcLength
  , arcLengthParam
  , C.splitBezier
  , colinear
  , evalBezier
  , evalBezierDeriv
  , bezierHoriz
  , bezierVert
  , C.bezierSubsegment
  , C.reorient
  , closedPathCurves
  , openPathCurves
  , curvesToClosed
  , closest
  , unmetaOpen
  , unmetaClosed
  , union
  , bezierIntersection
  , interpolateVector
  , vectorDistance
  , findBezierInflection
  , findBezierCusp
  ) where

import qualified Data.Vector.Unboxed as V
import qualified Geom2D.CubicBezier  as C
import           Graphics.SvgTree    (FillRule (..))
import           Linear.V2

------------------------------------------------------------
-- Data types

-- | A bezier curve of any degree.
newtype AnyBezier a = AnyBezier (V.Vector (V2 a))

-- | A cubic bezier curve.
data CubicBezier a = CubicBezier
  { cubicC0 :: !(V2 a)
  , cubicC1 :: !(V2 a)
  , cubicC2 :: !(V2 a)
  , cubicC3 :: !(V2 a)
  } deriving (Show, Eq)

-- | A quadratic bezier curve.
data QuadBezier a = QuadBezier
  { quadC0 :: !(V2 a)
  , quadC1 :: !(V2 a)
  , quadC2 :: !(V2 a)
  } deriving (Show, Eq)

-- | Open cubicbezier path.
data OpenPath a = OpenPath [(V2 a, PathJoin a)] (V2 a)
  deriving (Show, Eq)

-- | Closed cubicbezier path.
newtype ClosedPath a = ClosedPath [(V2 a, PathJoin a)]
  deriving (Show, Eq)

-- | Join two points with either a straight line or a bezier
--   curve with two control points.
data PathJoin a
  = JoinLine
  | JoinCurve (V2 a) (V2 a)
  deriving (Show, Eq)

-- | Closed meta path.
newtype ClosedMetaPath a = ClosedMetaPath [(V2 a, MetaJoin a)]
  deriving (Show, Eq)

-- | Open meta path
data OpenMetaPath a = OpenMetaPath [(V2 a, MetaJoin a)] (V2 a)
  deriving (Show, Eq)

-- | The tension value specifies how /tense/ the curve is.
--   A higher value means the curve approaches a line segment,
--   while a lower value means the curve is more round. Metafont
--   doesn't allow values below 3/4.
data Tension a
  = Tension
    { tensionValue :: a }
  | TensionAtLeast -- ^ Like Tension, but keep the segment inside the
                   --   bounding triangle defined by the control points,
                   --   if there is one.
    { tensionValue :: a }
  deriving (Functor, Foldable, Traversable, Eq, Show)

-- | Join two meta points with either a bezier curve or tension
--   contraints.
data MetaJoin a
  = MetaJoin
  { metaTypeL :: MetaNodeType a
  , tensionL  :: Tension a
  , tensionR  :: Tension a
  , metaTypeR :: MetaNodeType a
  }
  | Controls (V2 a) (V2 a)
  deriving (Show, Eq)

-- | Node constraint type.
data MetaNodeType a
  = Open
  | Curl { curlgamma :: a }
  | Direction { nodedir :: V2 a }
  deriving (Show, Eq)

------------------------------------------------------------
-- Methods

-- | Convert a quadratic bezier to a cubic bezier.
quadToCubic :: Fractional a => QuadBezier a -> CubicBezier a
quadToCubic = upCast . C.quadToCubic . downCast

-- | @arcLength c t tol@ finds the arclength of the bezier @c@ at @t@,
--   within given tolerance @tol@.
arcLength :: CubicBezier Double -> Double -> Double -> Double
arcLength bezier = C.arcLength (downCast bezier)

-- | @arcLengthParam c len tol@ finds the parameter where the curve @c@
--   has the arclength @len@, within tolerance @tol@.
arcLengthParam :: CubicBezier Double -> Double -> Double -> Double
arcLengthParam bezier = C.arcLengthParam (downCast bezier)

-- | Return @False@ if some points fall outside a line with a thickness of the given tolerance.
colinear :: CubicBezier Double -> Double -> Bool
colinear bezier = C.colinear (downCast bezier)

-- | Calculate a value on the bezier curve.
evalBezier :: (C.GenericBezier b, V.Unbox a, Fractional a) => b a -> a -> V2 a
evalBezier c p = upCast $ C.evalBezier c p

-- | Calculate a value and the first derivative on the curve.
evalBezierDeriv :: (V.Unbox a, Fractional a,C.GenericBezier b) => b a -> a -> (V2 a, V2 a)
evalBezierDeriv c p = upCast $ C.evalBezierDeriv c p

-- | Find the parameter where the bezier curve is horizontal.
bezierHoriz :: CubicBezier Double -> [Double]
bezierHoriz = C.bezierHoriz . downCast

-- | Find the parameter where the bezier curve is vertical.
bezierVert :: CubicBezier Double -> [Double]
bezierVert = C.bezierVert . downCast

-- | Create a normal path from a metapath.
unmetaOpen :: OpenMetaPath Double -> OpenPath Double
unmetaOpen = upCast . C.unmetaOpen . downCast

-- | Create a normal path from a metapath.
unmetaClosed :: ClosedMetaPath Double -> ClosedPath Double
unmetaClosed = upCast . C.unmetaClosed . downCast

-- | `O((n+m)*log(n+m))`, for n segments and m intersections.
--   Union of paths, removing overlap and rounding to the given tolerance.
union :: [ClosedPath Double] -> FillRule -> Double -> [ClosedPath Double]
union p fill tol = upCast (C.union (downCast p) (downCast fill) tol)

-- | Find the intersections between two Bezier curves, using the Bezier Clip algorithm.
--   Returns the parameters for both curves.
bezierIntersection :: CubicBezier Double -> CubicBezier Double -> Double -> [(Double, Double)]
bezierIntersection a b = C.bezierIntersection (downCast a) (downCast b)

-- | Find the closest value on the bezier to the given point, within tolerance.
--   Return the first value found.
closest :: CubicBezier Double -> V2 Double -> Double -> Double
closest c p = C.closest (downCast c) (downCast p)

-- | Return the closed path as a list of curves.
closedPathCurves :: Fractional a => ClosedPath a -> [CubicBezier a]
closedPathCurves = upCast . C.closedPathCurves . downCast

-- | Return the open path as a list of curves.
openPathCurves :: Fractional a => OpenPath a -> [CubicBezier a]
openPathCurves = upCast . C.openPathCurves . downCast

-- | Make an open path from a list of curves. The last control point of each curve is ignored.
curvesToClosed :: [CubicBezier a] -> ClosedPath a
curvesToClosed = upCast . C.curvesToClosed . downCast

-- | Interpolate between two vectors.
interpolateVector :: Num a => V2 a -> V2 a -> a -> V2 a
interpolateVector a b p = upCast $ C.interpolateVector (downCast a) (downCast b) p

-- | Distance between two vectors.
vectorDistance :: Floating a => V2 a -> V2 a -> a
vectorDistance a b = C.vectorDistance (downCast a) (downCast b)

-- | Find inflection points on the curve.
findBezierInflection :: CubicBezier Double -> [Double]
findBezierInflection = C.findBezierInflection . downCast

-- | Find the cusps of a bezier.
findBezierCusp :: CubicBezier Double -> [Double]
findBezierCusp = C.findBezierCusp . downCast

------------------------------------------------------------
-- Instances

instance C.GenericBezier QuadBezier where
  degree = C.degree . downCast
  toVector = C.toVector . downCast
  unsafeFromVector = upCast . C.unsafeFromVector

instance C.GenericBezier CubicBezier where
  degree = C.degree . downCast
  toVector = C.toVector . downCast
  unsafeFromVector = upCast . C.unsafeFromVector

instance C.GenericBezier AnyBezier where
  degree = C.degree . downCast
  toVector = C.toVector . downCast
  unsafeFromVector = upCast . C.unsafeFromVector

------------------------------------------------------------
-- Casting

class Cast a b | a -> b, b -> a where
  downCast :: a -> b
  upCast   :: b -> a

instance Cast a b => Cast [a] [b] where
  downCast = map downCast
  upCast = map upCast

instance (Cast a a', Cast b b') => Cast (a,b) (a',b') where
  downCast (a, b) = (downCast a, downCast b)
  upCast (a, b) = (upCast a, upCast b)

instance Cast (V2 a) (C.Point a) where
  downCast (V2 a b) = C.Point a b
  upCast (C.Point a b) = V2 a b

instance Cast FillRule C.FillRule where
  downCast FillEvenOdd = C.EvenOdd
  downCast FillNonZero = C.NonZero
  upCast C.EvenOdd = FillEvenOdd
  upCast C.NonZero = FillNonZero

instance Cast (CubicBezier a) (C.CubicBezier a) where
  downCast (CubicBezier a b c d) = C.CubicBezier
    (downCast a) (downCast b) (downCast c) (downCast d)
  upCast (C.CubicBezier a b c d) = CubicBezier
    (upCast a) (upCast b) (upCast c) (upCast d)

instance Cast (QuadBezier a) (C.QuadBezier a) where
  downCast (QuadBezier a b c) = C.QuadBezier
    (downCast a) (downCast b) (downCast c)
  upCast (C.QuadBezier a b c)= QuadBezier
    (upCast a) (upCast b) (upCast c)

instance V.Unbox a => Cast (AnyBezier a) (C.AnyBezier a) where
  downCast (AnyBezier arr) = C.AnyBezier $
    V.map (\(V2 a b) -> (a,b)) arr
  upCast (C.AnyBezier arr) = AnyBezier $
    V.map (uncurry V2) arr

instance Cast (MetaNodeType a) (C.MetaNodeType a) where
  downCast Open            = C.Open
  downCast (Curl gamma)    = C.Curl gamma
  downCast (Direction dir) = C.Direction (downCast dir)
  upCast C.Open            = Open
  upCast (C.Curl gamma)    = Curl gamma
  upCast (C.Direction dir) = Direction (upCast dir)

instance Cast (Tension a) (C.Tension a) where
  downCast (Tension v)        = C.Tension v
  downCast (TensionAtLeast v) = C.TensionAtLeast v
  upCast (C.Tension v)        = Tension v
  upCast (C.TensionAtLeast v) = TensionAtLeast v

instance Cast (MetaJoin a) (C.MetaJoin a) where
  downCast (MetaJoin tyL tL tR tyR) =
    C.MetaJoin (downCast tyL) (downCast tL) (downCast tR) (downCast tyR)
  downCast (Controls p1 p2) = C.Controls (downCast p1) (downCast p2)
  upCast (C.MetaJoin tyL tL tR tyR) =
    MetaJoin (upCast tyL) (upCast tL) (upCast tR) (upCast tyR)
  upCast (C.Controls p1 p2)         = Controls (upCast p1) (upCast p2)

instance Cast (PathJoin a) (C.PathJoin a) where
  downCast JoinLine        = C.JoinLine
  downCast (JoinCurve a b) = C.JoinCurve (downCast a) (downCast b)
  upCast C.JoinLine        = JoinLine
  upCast (C.JoinCurve a b) = JoinCurve (upCast a) (upCast b)

instance Cast (OpenMetaPath a) (C.OpenMetaPath a) where
  downCast (OpenMetaPath lst end) = C.OpenMetaPath
    [ (downCast p, downCast j)
    | (p, j) <- lst ] (downCast end)
  upCast (C.OpenMetaPath lst end) = OpenMetaPath
    [ (upCast p, upCast j)
    | (p, j) <- lst ] (upCast end)

instance Cast (ClosedMetaPath a) (C.ClosedMetaPath a) where
  downCast (ClosedMetaPath lst) = C.ClosedMetaPath
    [ (downCast p, downCast j)
    | (p, j) <- lst ]
  upCast (C.ClosedMetaPath lst) = ClosedMetaPath
    [ (upCast p, upCast j)
    | (p, j) <- lst ]

instance Cast (OpenPath a) (C.OpenPath a) where
  downCast (OpenPath lst end) = C.OpenPath
    [ (downCast p, downCast j)
    | (p, j) <- lst ] (downCast end)
  upCast (C.OpenPath lst end) = OpenPath
    [ (upCast p, upCast j)
    | (p, j) <- lst ] (upCast end)

instance Cast (ClosedPath a) (C.ClosedPath a) where
  downCast (ClosedPath lst) = C.ClosedPath
    [ (downCast p, downCast j)
    | (p, j) <- lst ]
  upCast (C.ClosedPath lst) = ClosedPath
    [ (upCast p, upCast j)
    | (p, j) <- lst ]

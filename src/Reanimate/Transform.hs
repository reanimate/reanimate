{-# LANGUAGE BangPatterns   #-}
{-|
  2D transformation matrices capable of translating, scaling,
  rotating, and skewing.
-}
module Reanimate.Transform
  ( identity
  , transformPoint
  , mkMatrix
  , toTransformation
  ) where

-- XXX: Use Linear.Matrix instead of Data.Matrix to drop the 'matrix' dependency.
import           Data.List
import           Data.Matrix (Matrix)
import qualified Data.Matrix as M
import           Data.Maybe
import           Graphics.SvgTree
import           Linear.V2

-- | Identity matrix.
--
--   @transformPoints identity x = x@
identity :: Matrix Coord
identity = M.identity 3

fromList :: [Coord] -> Matrix Coord
fromList [a,b,c,d,e,f] = M.fromList 3 3 [a,c,e,b,d,f,0,0,1]
fromList _             = error "Reanimate.Transform.fromList: bad input"

-- | Apply a transformation matrix to a 2D point.
transformPoint :: Matrix Coord -> RPoint -> RPoint
transformPoint m (V2 x y) = V2 (a*x +c*y + e) (b*x + d*y +f)
  where
    !a = M.unsafeGet 1 1 m
    !c = M.unsafeGet 1 2 m
    !e = M.unsafeGet 1 3 m
    !b = M.unsafeGet 2 1 m
    !d = M.unsafeGet 2 2 m
    !f = M.unsafeGet 2 3 m
    -- (a:c:e:b:d:f:_) = M.toList m

-- | Convert multiple SVG transformations into a single transformation matrix.
mkMatrix :: Maybe [Transformation] -> Matrix Coord
mkMatrix Nothing   = identity
mkMatrix (Just ts) = foldl' (*) identity (map transformationMatrix ts)

-- | Convert an SVG transformation into a transformation matrix.
transformationMatrix :: Transformation -> Matrix Coord
transformationMatrix transformation =
  case transformation of
    TransformMatrix a b c d e f -> fromList [a,b,c,d,e,f]
    Translate x y               -> translate x y
    Scale sx mbSy               -> fromList [sx,0,0,fromMaybe sx mbSy,0,0]
    Rotate a Nothing            -> rotate a
    Rotate a (Just (x,y))       -> translate x y * rotate a * translate (-x) (-y)
    SkewX a                     -> fromList [1,0,tan (a*pi/180),1,0,0]
    SkewY a                     -> fromList [1,tan (a*pi/180),0,1,0,0]
    TransformUnknown            -> identity
  where
    translate x y = fromList [1,0,0,1,x,y]
    rotate a = fromList [cos r,sin r,-sin r,cos r,0,0]
      where r = a * pi / 180

-- | Convert a transformation matrix back into an SVG transformation.
toTransformation :: Matrix Coord -> Transformation
toTransformation m = TransformMatrix a b c d e f
  where
    [a,c,e,b,d,f,_,_,_] = M.toList m

module Reanimate.Math.FloydWarshall where

import           Control.Monad (join)
import           Data.List
import           Data.Maybe
import qualified Data.Map      as Map
import           Data.Map (Map)

data Shortest b a = Shortest { distance :: a, path :: [b] }
  deriving Show

instance (Ord a, Eq b) => Semigroup (Shortest b a) where
  a <> b = case distance a `compare` distance b of
    GT -> b
    LT -> a
    EQ -> a { path = path a `union` path b }

floydWarshall :: (Semigroup b, Ord b, Ord p) =>
     [p]
     -> Map (p, p) (Shortest p b)
     -> Map (p, p) (Maybe (Shortest p b))
floydWarshall v dist = foldr innerCycle (Just <$> dist) v
  where
    innerCycle k dist = (newDist <$> v <*> v) `setTo` dist
      where
        newDist i j =
          ((i,j), do a <- join $ Map.lookup (i, k) dist
                     b <- join $ Map.lookup (k, j) dist
                     return $ Shortest (distance a <> distance b) (path a))
        setTo = Map.unionWith (<>) . Map.fromList

buildPaths :: Ord p => Map (p,p) (Shortest p b) -> Map (p,p) (Shortest [p] b)
buildPaths d = Map.mapWithKey (\pair s -> s { path = buildPath pair}) d
  where
    buildPath (i,j)
      | i == j = [[j]]
      | otherwise = do
        k <- path $ fromJust $ Map.lookup (i,j) d
        p <- buildPath (k, j)
        [i : p]

findMinDistances :: (Monoid b, Ord p, Ord b) => [p] -> Map (p,p) b -> Map (p,p) (Shortest [p] b)
findMinDistances v g =
  let weights = Map.mapWithKey (\(_,j) w -> Shortest w [j]) g
      trivial = Map.fromList [ ((i,i), Shortest mempty []) | i <- v ]
      clean d = fromJust <$> Map.filter isJust (d Map.\\ trivial)
  in buildPaths $ clean $ floydWarshall v (weights <> trivial)

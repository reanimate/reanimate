module Reanimate.Combinators where

type Path = [(Double, Double)]

approxFnData :: Int -> (Double -> (Double, Double)) -> Path
approxFnData steps fn =
  fn 0 : [ fn (fromIntegral n/fromIntegral steps) | n <- [0..steps] ]

morphPath :: Path -> Path -> Double -> Path
morphPath src dst idx = zipWith worker src dst
  where
    worker (x1, y1) (x2, y2) =
      (x1 + (x2-x1)*idx
      ,y1 + (y2-y1)*idx)

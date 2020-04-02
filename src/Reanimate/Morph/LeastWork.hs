module Reanimate.Morph.LeastWork where

stretchWork :: Double -> Double -> Double -> Double -> Double
stretchWork c e lenStart lenEnd =
    (delta**e)/((1-c)*lenMin + c*lenMax)
  where
    lenMin = min lenStart lenEnd
    lenMax = max lenStart lenEnd
    delta = lenEnd-lenStart

-- bendWork a b c
{-
quad d0 d1 d2 t = (d0+d2-2*d1)*t^2 + (2*d1-2*d0)*t + d0

a = (d0+d2-2*d1)
b = (2*d1-2*d0)
c = d0

x = -(2*d1-2*d0) +- sqrt ((2*d1-2*d0)^2 - (4*d0^2 + 4*d2*d0 - 4*2*d1*d0))
    / (2*(d0+d2-2*d1))
-}

{-# LANGUAGE OverloadedStrings, Arrows #-}
module Reanimate.Arrow where

import Control.Arrow
import qualified Control.Category as C
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Data.Fixed
import Lucid.Svg
import Lucid ()

data Animation a b = Animation Double (a -> Svg b)

instance C.Category Animation where
  id = Animation 0 pure
  Animation a fn1 . Animation b fn2 = Animation (max a b) (\a -> fn2 a >>= fn1)

instance Arrow Animation where
  -- arr :: (b -> c) -> Animation b c
  arr fn = Animation 0 (pure . fn)
  -- first :: Animation b c -> Animation (b, d) (c, d)
  first (Animation duration fn) =
    Animation duration (\(b,d) -> do c <- fn b; pure (c, d))

type Ani a = Animation Double a

duration :: Double -> Animation a ()
duration duration = Animation duration (\_ -> pure ())

getDuration :: Animation a b -> Double
getDuration (Animation d _) = d

frameAt :: Double -> Ani () -> Svg ()
frameAt n (Animation d fn) = svg $ fn (n `mod'` d)
  where
    svg :: Svg () -> Svg ()
    svg content = do
      doctype_
      with (svg11_ content) [width_ "320" , height_ "180", viewBox_ "0 0 320 180"]

emit :: Animation (Svg ()) ()
emit = Animation 0 id

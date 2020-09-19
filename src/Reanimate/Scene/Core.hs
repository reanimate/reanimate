{-# LANGUAGE RankNTypes #-}

module Reanimate.Scene.Core where

import Control.Monad.Fix (MonadFix (..))
import Control.Monad.ST
import Data.List
import Reanimate.Animation
import Reanimate.Svg.Constructors

-- | The ZIndex property specifies the stack order of sprites and animations. Elements
--   with a higher ZIndex will be drawn on top of elements with a lower index.
type ZIndex = Int

-- (seq duration, par duration)
-- [(Time, Animation, ZIndex)]
-- Map Time [(Animation, ZIndex)]
type Gen s = ST s (Duration -> Time -> (SVG, ZIndex))

-- | A 'Scene' represents a sequence of animations and variables
--   that change over time.
newtype Scene s a = M {unM :: Time -> ST s (a, Duration, Duration, [Gen s])}

instance Functor (Scene s) where
  fmap f action = M $ \t -> do
    (a, d1, d2, gens) <- unM action t
    return (f a, d1, d2, gens)

instance Applicative (Scene s) where
  pure a = M $ \_ -> return (a, 0, 0, [])
  f <*> g = M $ \t -> do
    (f', s1, p1, gen1) <- unM f t
    (g', s2, p2, gen2) <- unM g (t + s1)
    return (f' g', s1 + s2, max p1 (s1 + p2), gen1 ++ gen2)

instance Monad (Scene s) where
  return = pure
  f >>= g = M $ \t -> do
    (a, s1, p1, gen1) <- unM f t
    (b, s2, p2, gen2) <- unM (g a) (t + s1)
    return (b, s1 + s2, max p1 (s1 + p2), gen1 ++ gen2)

instance MonadFix (Scene s) where
  mfix fn = M $ \t -> mfix (\v -> let (a, _s, _p, _gens) = v in unM (fn a) t)

-- | Lift ST action into the Scene monad.
liftST :: ST s a -> Scene s a
liftST action = M $ \_ -> action >>= \a -> return (a, 0, 0, [])

-- | Evaluate the value of a scene.
evalScene :: (forall s. Scene s a) -> a
evalScene action = runST $ do
  (val, _, _, _) <- unM action 0
  return val

-- | Render a 'Scene' to an 'Animation'.
scene :: (forall s. Scene s a) -> Animation
scene action =
  runST
    ( do
        (_, s, p, gens) <- unM action 0
        let dur = max s p
        genFns <- sequence gens
        return $
          mkAnimation
            dur
            ( \t ->
                mkGroup $
                  map fst $
                    sortOn
                      snd
                      [spriteRender dur (t * dur) | spriteRender <- genFns]
            )
    )

-- | Execute actions in a scene without advancing the clock. Note that scenes do not end before
--   all forked actions have completed.
--
--   Example:
--
-- @
-- do 'fork' $ 'Reanimate.Scene.play' 'Reanimate.Builtin.Documentation.drawBox'
--    'Reanimate.Scene.play' 'Reanimate.Builtin.Documentation.drawCircle'
-- @
--
--   <<docs/gifs/doc_fork.gif>>
fork :: Scene s a -> Scene s a
fork (M action) = M $ \t -> do
  (a, s, p, gens) <- action t
  return (a, 0, max s p, gens)

-- | Query the current clock timestamp.
--
--   Example:
--
-- @
-- do now \<- 'Reanimate.Scene.play' 'Reanimate.Builtin.Documentation.drawCircle' *\> 'queryNow'
--    'Reanimate.Scene.play' $ 'staticFrame' 1 $ 'scale' 2 $ 'withStrokeWidth' 0.05 $
--      'mkText' $ "Now=" <> T.pack (show now)
-- @
--
--   <<docs/gifs/doc_queryNow.gif>>
queryNow :: Scene s Time
queryNow = M $ \t -> return (t, 0, 0, [])

-- | Advance the clock by a given number of seconds.
--
--   Example:
--
-- @
-- do 'fork' $ 'Reanimate.Scene.play' 'Reanimate.Builtin.Documentation.drawBox'
--    'wait' 1
--    'Reanimate.Scene.play' 'Reanimate.Builtin.Documentation.drawCircle'
-- @
--
--   <<docs/gifs/doc_wait.gif>>
wait :: Duration -> Scene s ()
wait d = M $ \_ -> return ((), d, 0, [])

-- | Wait until the clock is equal to the given timestamp.
waitUntil :: Time -> Scene s ()
waitUntil tNew = do
  now <- queryNow
  wait (max 0 (tNew - now))

-- | Wait until all forked and sequential animations have finished.
--
--   Example:
--
-- @
-- do 'waitOn' $ 'fork' $ 'Reanimate.Scene.play' 'Reanimate.Builtin.Documentation.drawBox'
--    'Reanimate.Scene.play' 'Reanimate.Builtin.Documentation.drawCircle'
-- @
--
--   <<docs/gifs/doc_waitOn.gif>>
waitOn :: Scene s a -> Scene s a
waitOn (M action) = M $ \t -> do
  (a, s, p, gens) <- action t
  return (a, max s p, 0, gens)

-- | Change the ZIndex of a scene.
adjustZ :: (ZIndex -> ZIndex) -> Scene s a -> Scene s a
adjustZ fn (M action) = M $ \t -> do
  (a, s, p, gens) <- action t
  return (a, s, p, map genFn gens)
  where
    genFn gen = do
      frameGen <- gen
      return $ \d t -> let (svg, z) = frameGen d t in (svg, fn z)

-- | Query the duration of a scene.
withSceneDuration :: Scene s () -> Scene s Duration
withSceneDuration s = do
  t1 <- queryNow
  s
  t2 <- queryNow
  return (t2 - t1)

addGen :: Gen s -> Scene s ()
addGen gen = M $ \_ -> return ((), 0, 0, [gen])

{-# LANGUAGE RankNTypes #-}
module Reanimate.Scene where

import           Control.Monad.Fix
import           Control.Monad.ST
import           Data.List
import           Data.Ord
import           Debug.Trace
import           Reanimate.Monad
import           Reanimate.Svg

data World
type ZIndex = Int

(#) :: a -> (a -> b) -> b
o # f = f o

-- (seq duration, par duration)
-- [(Time, Animation, ZIndex)]
-- Map Time [(Animation, ZIndex)]
type Timeline = [(Time, Animation, ZIndex)]
newtype Scene s a = M { unM :: Time -> ST s (a, Duration, Duration, Timeline) }

unionTimeline :: Timeline -> Timeline -> Timeline
unionTimeline = (++)

emptyTimeline :: Timeline
emptyTimeline = []

instance Functor (Scene s) where
  fmap f action = M $ \t -> do
    (a, d1, d2, tl) <- unM action t
    return (f a, d1, d2, tl)

instance Applicative (Scene s) where
  pure a = M $ \_ -> return (a, 0, 0, emptyTimeline)
  f <*> g = M $ \t -> do
    (f', s1, p1, tl1) <- unM f t
    (g', s2, p2, tl2) <- unM g (t+s1)
    return (f' g', s1+s2, max p1 (s1+p2), unionTimeline tl1 tl2)

instance Monad (Scene s) where
  return = pure
  f >>= g = M $ \t -> do
    (a, s1, p1, tl1) <- unM f t
    (b, s2, p2, tl2) <- unM (g a) (t+s1)
    return (b, s1+s2, max p1 (s1+p2), unionTimeline tl1 tl2)

instance MonadFix (Scene s) where
  mfix fn = M $ \t -> mfix (\v -> let (a,_s,_p,_tl) = v in unM (fn a) t)

--data Frame a = Frame {unFrame :: Duration -> Time -> State ([Tree] -> [Tree]) a}
sceneAnimation :: (forall s. Scene s a) -> Animation
sceneAnimation action = Animation (max s p) $ \t ->
  mkGroup $ map snd $ sortBy (comparing fst)
    [ (z, frameGen (t-startT))
    | (startT, Animation dur frameGen, z) <- tl
    , t >= startT
    , t < startT+dur
    ]
  where
    (_, s, p, tl) = runST (unM action 0)

debug :: String -> Scene s ()
debug msg = M $ \_ -> trace msg (return ((), 0, 0, emptyTimeline))

someaction :: Scene s ()
someaction = debug "someaction"

fork :: Scene s a -> Scene s a
fork (M action) = M $ \t -> do
  (a, s, p, tl) <- action t
  return (a, 0, max s p, tl)

play :: Animation -> Scene s ()
play = playZ 0

playZ :: ZIndex -> Animation -> Scene s ()
playZ z ani = M $ \t -> do
  let d = duration ani
  return ((), d, 0, [(t, ani, z)])

queryNow :: Scene s Time
queryNow = M $ \t -> return (t, 0, 0, emptyTimeline)

-- Wait until all forked and sequential animations have finished.
waitAll :: Scene s a -> Scene s a
waitAll (M action) = M $ \t -> do
  (a, s, p, tl) <- action t
  return (a, max s p, 0, tl)

waitUntil :: Time -> Scene s ()
waitUntil tNew = do
  now <- queryNow
  wait (max 0 (tNew - now))

wait :: Duration -> Scene s ()
wait d = M $ \_ ->
  return ((), d, 0, emptyTimeline)

adjustZ :: (ZIndex -> ZIndex) -> Scene s a -> Scene s a
adjustZ fn (M action) = M $ \t -> do
  (a, s, p, tl) <- action t
  return (a, s, p, [ (startT, ani, fn z) | (startT, ani, z) <- tl ])

withSceneDuration :: Scene s () -> Scene s Duration
withSceneDuration s = do
  t1 <- queryNow
  s
  t2 <- queryNow
  return (t2-t1)

{-
blackness
show numbers
fade in colormap
slide to middle

do fork $ play $ setDuration 5 showNumbers
      # fadeOut 1 # fadeIn 1
   wait 3

   fork $ do
    playZ (-1) $ setDuration 5 $ revealImage 0 0.5
      # pauseAround 1 1
    playZ (-1) $ setDuration 5 $ revealImage 0.5 1
      # pauseAtEnd 1

   play $ setDuration 5 $ colormap 0 0.5
     # fadeIn 1 # pauseAround 1 1
   play $ setDuration 5 $ colormap 0.5 1
     # fadeOut 1 # pauseAtEnd 1

-}

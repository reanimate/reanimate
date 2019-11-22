{-# LANGUAGE RankNTypes #-}
module Reanimate.Scene where

import           Control.Monad.Fix
import           Control.Monad.ST
import           Data.List
import           Data.Ord
import           Data.STRef
import           Reanimate.Animation
import           Reanimate.Signal
import           Reanimate.Effect
import           Reanimate.Svg.Constructors
import           Graphics.SvgTree (Tree(None))

type ZIndex = Int

(#) :: a -> (a -> b) -> b
o # f = f o

-- (seq duration, par duration)
-- [(Time, Animation, ZIndex)]
-- Map Time [(Animation, ZIndex)]
type Timeline = [(Time, Animation, ZIndex)]
type Gen s = ST s (Duration -> Time -> (SVG, ZIndex))
newtype Scene s a = M { unM :: Time -> ST s (a, Duration, Duration, Timeline, [Gen s]) }

unionTimeline :: Timeline -> Timeline -> Timeline
unionTimeline = (++)

emptyTimeline :: Timeline
emptyTimeline = []

instance Functor (Scene s) where
  fmap f action = M $ \t -> do
    (a, d1, d2, tl, gens) <- unM action t
    return (f a, d1, d2, tl, gens)

instance Applicative (Scene s) where
  pure a = M $ \_ -> return (a, 0, 0, emptyTimeline, [])
  f <*> g = M $ \t -> do
    (f', s1, p1, tl1, gen1) <- unM f t
    (g', s2, p2, tl2, gen2) <- unM g (t+s1)
    return (f' g', s1+s2, max p1 (s1+p2), unionTimeline tl1 tl2, gen1++gen2)

instance Monad (Scene s) where
  return = pure
  f >>= g = M $ \t -> do
    (a, s1, p1, tl1, gen1) <- unM f t
    (b, s2, p2, tl2, gen2) <- unM (g a) (t+s1)
    return (b, s1+s2, max p1 (s1+p2), unionTimeline tl1 tl2, gen1++gen2)

instance MonadFix (Scene s) where
  mfix fn = M $ \t -> mfix (\v -> let (a,_s,_p,_tl,_gens) = v in unM (fn a) t)

liftST :: ST s a -> Scene s a
liftST action = M $ \_ -> action >>= \a -> return (a, 0, 0, emptyTimeline, [])

sceneAnimation :: (forall s. Scene s a) -> Animation
sceneAnimation action =
  runST (do
    (_, s, p, tl, gens) <- unM action 0
    let dur = max s p
        anis = foldl' parDropA (pause 0) $
                map snd $ sortBy (comparing fst)
                  [ (z, pause startT `seqA` a)
                  | (startT, a, z) <- tl
                  ]
    genFns <- sequence gens
    return $ anis `parA` mkAnimation dur (\t ->
      mkGroup $
      map fst $
      sortBy (comparing snd)
      [ spriteRender dur (t*dur)
      | spriteRender <- genFns ])
  )

fork :: Scene s a -> Scene s a
fork (M action) = M $ \t -> do
  (a, s, p, tl, gens) <- action t
  return (a, 0, max s p, tl, gens)

play :: Animation -> Scene s ()
play = playZ 0

playZ :: ZIndex -> Animation -> Scene s ()
playZ z ani = M $ \t -> do
  let d = duration ani
  return ((), d, 0, [(t, ani, z)], [])

queryNow :: Scene s Time
queryNow = M $ \t -> return (t, 0, 0, emptyTimeline, [])

-- Wait until all forked and sequential animations have finished.
waitAll :: Scene s a -> Scene s a
waitAll (M action) = M $ \t -> do
  (a, s, p, tl, gens) <- action t
  return (a, max s p, 0, tl, gens)

waitUntil :: Time -> Scene s ()
waitUntil tNew = do
  now <- queryNow
  wait (max 0 (tNew - now))

wait :: Duration -> Scene s ()
wait d = M $ \_ ->
  return ((), d, 0, emptyTimeline, [])

adjustZ :: (ZIndex -> ZIndex) -> Scene s a -> Scene s a
adjustZ fn (M action) = M $ \t -> do
  (a, s, p, tl, gens) <- action t
  return (a, s, p, [ (startT, ani, fn z) | (startT, ani, z) <- tl ], gens)

withSceneDuration :: Scene s () -> Scene s Duration
withSceneDuration s = do
  t1 <- queryNow
  s
  t2 <- queryNow
  return (t2-t1)

data Object s = Object (STRef s (Maybe Timeline))

newObject :: Scene s (Object s)
newObject = Object <$> liftST (newSTRef Nothing)

stretchTimeline :: Timeline -> Scene s ()
stretchTimeline = mapM_ worker
  where
    worker (t, a, z) = M $ \tNow -> -- 3
      let tNew = t + duration a -- 1+1=2
          dNew = tNow - tNew -- 3-2=1
          aNew = setDuration dNew (signalA (constantS 1) a) in
      if (dNew > 0)
        then return ((), 0, 0, [(tNew, aNew, z)], [])
        else return ((), 0, 0, emptyTimeline, [])

dropObject :: Object s -> Scene s ()
dropObject (Object ref) = do
  mbTimeline <- liftST $ readSTRef ref
  case mbTimeline of
    Nothing -> return ()
    Just timeline -> do
      liftST $ writeSTRef ref Nothing
      stretchTimeline timeline

listen :: Scene s a -> Scene s (a, Timeline)
listen scene = M $ \t -> do
  (a, s, p, tl, gens) <- unM scene t
  return ((a,tl), s, p, tl, gens)

withObject :: Object s -> Scene s a -> Scene s a
withObject obj@(Object ref) scene = do
  dropObject obj
  (a, tl) <- listen scene
  liftST $ writeSTRef ref (Just tl)
  return a


data Param s a = Param (STRef s (Time, Time, Time -> a))

newParam :: a -> Scene s (Param s a)
newParam initVal = do
  now <- queryNow
  Param <$> liftST (newSTRef (now, -1, const initVal))

destroyParam :: Param s a -> Scene s ()
destroyParam (Param ref) = do
  now <- queryNow
  liftST $ do
    (startT, _endT, fn) <- readSTRef ref
    writeSTRef ref (startT, now, fn)

readParam :: Param s a -> Scene s a
readParam (Param ref) = do
  now <- queryNow
  (_, _, fn) <- liftST $ readSTRef ref
  return $ fn now

paramAt :: Param s a -> Time -> ST s a
paramAt = undefined

paramFn :: Param s a -> ST s (Time -> a)
paramFn (Param ref) = do
  (_, _, fn) <- readSTRef ref
  return fn

withParamAt :: Param s a -> Time -> (a -> ST s SVG) -> ST s SVG
withParamAt = undefined

fromParams :: Gen s -> Scene s ()
fromParams gen = M $ \_ -> return ((), 0, 0, emptyTimeline, [gen])

setParam :: Param s a -> a -> Scene s ()
setParam = undefined

-- setParamZIndex :: Param s a -> ZIndex -> Scene s ()
-- setParamZIndex = undefined
--
-- adjustParamZIndex :: Param s a -> Duration -> (Time -> ZIndex -> ZIndex) -> Scene s ()
-- adjustParamZIndex = undefined
--
-- adjustParamZIndex_ :: Param s a -> (Time -> ZIndex -> ZIndex) -> Scene s ()
-- adjustParamZIndex_ = undefined

tweenParam :: Param s a -> Duration -> (Double -> a -> a) -> Scene s ()
tweenParam (Param ref) dur fn = do
  now <- queryNow
  liftST $ do
    (startT,endT,prevFn) <- readSTRef ref
    let worker t
          | t > now = fn (min 1 ((t-now)/dur)) (prevFn t)
          | otherwise = prevFn t
    writeSTRef ref (startT, endT, worker)
  wait dur

tweenParam_ :: Param s a -> (Time -> a -> a) -> Scene s ()
tweenParam_ = undefined

simpleParam :: (a -> SVG) -> a -> Scene s (Param s a)
simpleParam render def = do
  p <- newParam def
  fromParams $ do
    fn <- paramFn p
    return $ \_d t -> (render $ fn t, 0)
  return p

data Var s a = Var (STRef s (Time -> a))

newVar :: a -> Scene s (Var s a)
newVar def = Var <$> liftST (newSTRef (const def))

readVar :: Var s a -> Scene s a
readVar (Var ref) = liftST (readSTRef ref) <*> queryNow

writeVar :: Var s a -> a -> Scene s ()
writeVar var val = modifyVar var (const val)

modifyVar :: Var s a -> (a -> a) -> Scene s ()
modifyVar (Var ref) fn = do
  now <- queryNow
  liftST $ modifySTRef ref $ \prev t ->
    if t < now
      then prev t
      else fn (prev t)

tweenVar :: Var s a -> Duration -> (Time -> a -> a) -> Scene s ()
tweenVar (Var ref) dur fn = do
  now <- queryNow
  liftST $ modifySTRef ref $ \prev t ->
    if t < now
      then prev t
      else fn (min dur $ t-now) $ prev t

freezeVar :: Var s a -> ST s (Time -> a)
freezeVar (Var ref) = readSTRef ref

findVar :: (a -> Bool) -> [Var s a] -> Scene s (Var s a)
findVar _cond [] = error "Variable not found."
findVar cond (v:vs) = do
  val <- readVar v
  if cond val then return v else findVar cond vs

applyVar :: Var s a -> Sprite s -> (a -> SVG -> SVG) -> Scene s ()
applyVar var sprite fn = do
  spriteModify sprite $ do
    varFn <- freezeVar var
    return $ \absT _relD _relT (svg, zindex) ->
      (fn (varFn absT) svg, zindex)

data Sprite s = Sprite Time (STRef s (Duration, ST s (Duration -> Time -> SVG -> (SVG, ZIndex))))

newSprite :: ST s (Time -> Duration -> Time -> SVG) -> Scene s (Sprite s)
newSprite render = do
  now <- queryNow
  ref <- liftST $ newSTRef (-1, return $ \_d _t svg -> (svg, 0))
  fromParams $ do
    fn <- render
    (spriteDuration, spriteEffectGen) <- readSTRef ref
    spriteEffect <- spriteEffectGen
    return $ \d absT ->
      let relD = (if spriteDuration < 0 then d else spriteDuration)-now
          relT = absT-now in
      if relT < 0 || relD < relT
        then (None, 0)
        else spriteEffect relD relT (fn absT relD relT)
  return $ Sprite now ref

newSpriteA :: Animation -> Scene s (Sprite s)
newSpriteA (Animation _ gen) = do
  now <- queryNow
  ref <- liftST $ newSTRef (-1, return $ \_d _t svg -> (svg, 0))
  fromParams $ do
    (spriteDuration, spriteEffectGen) <- readSTRef ref
    spriteEffect <- spriteEffectGen
    return $ \d absT ->
      let relD = (if spriteDuration < 0 then d else spriteDuration)-now
          relT = absT-now in
      if relT < 0 || relT > relD
        then (None, 0)
        else spriteEffect relD relT (gen (relT/relD))
  return $ Sprite now ref

destroySprite :: Sprite s -> Scene s ()
destroySprite (Sprite _ ref) = do
  now <- queryNow
  liftST $ modifySTRef ref $ \(ttl, render) ->
    (if ttl < 0 then now else min ttl now, render)

spriteModify :: Sprite s -> ST s (Time -> Duration -> Time -> (SVG, ZIndex) -> (SVG, ZIndex)) -> Scene s ()
spriteModify (Sprite born ref) modFn =
  liftST $ modifySTRef ref $ \(ttl, renderGen) ->
    (ttl, do
      render <- renderGen
      modRender <- modFn
      return $ \relD relT ->
        let absT = relT + born
        in modRender absT relD relT . render relD relT)

spriteE :: Sprite s -> Effect -> Scene s ()
spriteE (Sprite born ref) effect = do
  now <- queryNow
  liftST $ modifySTRef ref $ \(ttl, renderGen) ->
    (ttl, do
      render <- renderGen
      return $ \d t svg ->
        let (svg', z) = render d t svg
        in (delayE (now-born) effect d t svg', z))

spriteZ :: Sprite s -> ZIndex -> Scene s ()
spriteZ (Sprite born ref) zindex = do
  now <- queryNow
  liftST $ modifySTRef ref $ \(ttl, renderGen) ->
    (ttl, do
      render <- renderGen
      return $ \d t svg ->
        let (svg', z) = render d t svg
        in (svg', if t < now-born then z else zindex))

{-
data Var s a = Var (STRef s (Time -> a))
data Sprite s = Sprite (STRef s (Duration, Duration -> Time -> (SVG, ZIndex)))

newVar :: a -> Scene s (Var s a)
readVar :: Var s a -> Scene s a
writeVar :: Var s a -> a -> Scene s ()
modifyVar :: Var s a -> (a -> a) -> Scene s ()
freezeVar :: Var s a -> ST s (Time -> a)

newSprite :: ST s (Time -> Time -> SVG) -> Scene s (Sprite s)
destroySprite :: Sprite s -> Scene s ()
spriteE :: Sprite s -> Effect -> Scene s ()

newBlock :: Var s Position -> Number -> Scene s (Sprite s)
-}

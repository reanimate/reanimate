{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Reanimate.Scene.Sprite where

import Control.Monad (void)
import Control.Monad.ST (ST)
import Data.Bifunctor (Bifunctor (first))
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import Graphics.SvgTree
  ( pattern None,
  )
import Reanimate.Animation
  ( Animation,
    Duration,
    SVG,
    Sync (SyncStretch),
    Time,
    dropA,
    duration,
    getAnimationFrame,
  )
import Reanimate.Effect (Effect, delayE)
import Reanimate.Scene.Core
  ( Scene (M),
    ZIndex,
    addGen,
    fork,
    liftST,
    queryNow,
    scene,
    wait,
  )
import Reanimate.Scene.Var (unpackVar, Var (..), newVar, readVar)
import Reanimate.Transition (Transition, overlapT)

-- | Create and render a variable. The rendering will be born at the current timestamp
--   and will persist until the end of the scene.
--
--   Example:
--
-- @
-- do var \<- 'simpleVar' 'Reanimate.Svg.Constructors.mkCircle' 0
--    'Reanimate.Scene.tweenVar' var 2 $ \\val -> 'Reanimate.fromToS' val ('Reanimate.Constants.screenHeight'/2)
-- @
--
--   <<docs/gifs/doc_simpleVar.gif>>
simpleVar :: (a -> SVG) -> a -> Scene s (Var s a)
simpleVar render def = do
  v <- newVar def
  _ <- newSprite $ render <$> unVar v
  return v

-- | Helper function for filtering variables.
findVar :: (a -> Bool) -> [Var s a] -> Scene s (Var s a)
findVar _cond [] = error "Variable not found."
findVar cond (v : vs) = do
  val <- readVar v
  if cond val then return v else findVar cond vs

-- | Play an animation once and then remove it. This advances the clock by the duration of the
--   animation.
--
--   Example:
--
-- @
-- do 'play' 'Reanimate.Builtin.Documentation.drawBox'
--    'play' 'Reanimate.Builtin.Documentation.drawCircle'
-- @
--
--   <<docs/gifs/doc_play.gif>>
play :: Animation -> Scene s ()
play ani = newSpriteA ani >>= destroySprite

-- | Sprites are animations with a given time of birth as well as a time of death.
--   They can be controlled using variables, tweening, and effects.
data Sprite s = Sprite Time (STRef s (Duration, ST s (Duration -> Time -> SVG -> (SVG, ZIndex))))

-- | Sprite frame generator. Generates frames over time in a stateful environment.
newtype Frame s a = Frame {unFrame :: ST s (Time -> Duration -> Time -> a)}

instance Functor (Frame s) where
  fmap fn (Frame gen) = Frame $ do
    m <- gen
    return (\real_t d t -> fn $ m real_t d t)

instance Applicative (Frame s) where
  pure v = Frame $ return (\_ _ _ -> v)
  Frame f <*> Frame g = Frame $ do
    m1 <- f
    m2 <- g
    return $ \real_t d t -> m1 real_t d t (m2 real_t d t)

-- | Dereference a variable as a Sprite frame.
--
--   Example:
--
-- @
-- do v \<- 'newVar' 0
--    'newSprite' $ 'Reanimate.Svg.Constructors.mkCircle' \<$\> 'unVar' v
--    'Reanimate.Scene.tweenVar' v 1 $ \\val -> 'Reanimate.fromToS' val 3
--    'Reanimate.Scene.tweenVar' v 1 $ \\val -> 'Reanimate.fromToS' val 0
-- @
--
--   <<docs/gifs/doc_unVar.gif>>
unVar :: Var s a -> Frame s a
unVar var = Frame $ do
  fn <- unpackVar var
  return $ \real_t _d _t -> fn real_t

-- | Dereference seconds since sprite birth.
spriteT :: Frame s Time
spriteT = Frame $ return (\_real_t _d t -> t)

-- | Dereference duration of the current sprite.
spriteDuration :: Frame s Duration
spriteDuration = Frame $ return (\_real_t d _t -> d)

-- | Create new sprite defined by a frame generator. Unless otherwise specified using
--   'destroySprite', the sprite will die at the end of the scene.
--
--   Example:
--
-- @
-- do 'newSprite' $ 'Reanimate.Svg.Constructors.mkCircle' \<$\> 'spriteT' -- Circle sprite where radius=time.
--    'wait' 2
-- @
--
--   <<docs/gifs/doc_newSprite.gif>>
newSprite :: Frame s SVG -> Scene s (Sprite s)
newSprite render = do
  now <- queryNow
  ref <- liftST $ newSTRef (-1, return $ \_d _t svg -> (svg, 0))
  addGen $ do
    fn <- unFrame render
    (spriteDur, spriteEffectGen) <- readSTRef ref
    spriteEffect <- spriteEffectGen
    return $ \d absT ->
      let relD = (if spriteDur < 0 then d else spriteDur) - now
          relT = absT - now
          -- Sprite is live [now;duration[
          -- If we're at the end of a scene, sprites
          -- are live: [now;duration]
          -- This behavior is difficult to get right. See the 'bug_*' examples for
          -- automated tests.
          inTimeSlice = relT >= 0 && relT < relD
          isLastFrame = d == absT && relT == relD
       in if inTimeSlice || isLastFrame
            then spriteEffect relD relT (fn absT relD relT)
            else (None, 0)
  return $ Sprite now ref

-- | Create new sprite defined by a frame generator. The sprite will die at
--   the end of the scene.
newSprite_ :: Frame s SVG -> Scene s ()
newSprite_ = void . newSprite

-- | Create a new sprite from an animation. This advances the clock by the
--   duration of the animation. Unless otherwise specified using
--   'destroySprite', the sprite will die at the end of the scene.
--
--   Note: If the scene doesn't end immediately after the duration of the
--   animation, the animation will be stretched to match the lifetime of the
--   sprite. See 'newSpriteA'' and 'play'.
--
--   Example:
--
-- @
-- do 'fork' $ 'newSpriteA' 'Reanimate.Builtin.Documentation.drawCircle'
--    'play' 'Reanimate.Builtin.Documentation.drawBox'
--    'play' $ 'Reanimate.Animation.reverseA' 'Reanimate.Builtin.Documentation.drawBox'
-- @
--
--   <<docs/gifs/doc_newSpriteA.gif>>
newSpriteA :: Animation -> Scene s (Sprite s)
newSpriteA = newSpriteA' SyncStretch

-- | Create a new sprite from an animation and specify the synchronization policy. This advances
--   the clock by the duration of the animation.
--
--   Example:
--
-- @
-- do 'fork' $ 'newSpriteA'' 'Reanimate.Animation.SyncFreeze' 'Reanimate.Builtin.Documentation.drawCircle'
--    'play' 'Reanimate.Builtin.Documentation.drawBox'
--    'play' $ 'Reanimate.Animation.reverseA' 'Reanimate.Builtin.Documentation.drawBox'
-- @
--
--   <<docs/gifs/doc_newSpriteA'.gif>>
newSpriteA' :: Sync -> Animation -> Scene s (Sprite s)
newSpriteA' sync animation =
  newSprite (getAnimationFrame sync animation <$> spriteT <*> spriteDuration)
    <* wait (duration animation)

-- | Create a sprite from a static SVG image.
--
--   Example:
--
-- @
-- do 'newSpriteSVG' $ 'Reanimate.Svg.Constructors.mkBackground' "lightblue"
--    'play' 'Reanimate.Builtin.Documentation.drawCircle'
-- @
--
--   <<docs/gifs/doc_newSpriteSVG.gif>>
newSpriteSVG :: SVG -> Scene s (Sprite s)
newSpriteSVG = newSprite . pure

-- | Create a permanent sprite from a static SVG image. Same as `newSpriteSVG`
--   but the sprite isn't returned and thus cannot be destroyed.
newSpriteSVG_ :: SVG -> Scene s ()
newSpriteSVG_ = void . newSpriteSVG

-- | Change the rendering of a sprite using data from a variable. If data from several variables
--   is needed, use a frame generator instead.
--
--   Example:
--
-- @
-- do s \<- 'fork' $ 'newSpriteA' 'Reanimate.Builtin.Documentation.drawBox'
--    v \<- 'newVar' 0
--    'applyVar' v s 'Reanimate.Svg.Constructors.rotate'
--    'Reanimate.Scene.tweenVar' v 2 $ \\val -> 'Reanimate.fromToS' val 90
-- @
--
--   <<docs/gifs/doc_applyVar.gif>>
applyVar :: Var s a -> Sprite s -> (a -> SVG -> SVG) -> Scene s ()
applyVar var sprite fn = spriteModify sprite $ do
  varFn <- unVar var
  return $ first $ fn varFn

-- | Destroy a sprite, preventing it from being rendered in the future of the scene.
--   If 'destroySprite' is invoked multiple times, the earliest time-of-death is used.
--
--   Example:
--
-- @
-- do s <- 'newSpriteSVG' $ 'Reanimate.Svg.Constructors.withFillOpacity' 1 $ 'Reanimate.Svg.Constructors.mkCircle' 1
--    'fork' $ 'wait' 1 \>\> 'destroySprite' s
--    'play' 'Reanimate.Builtin.Documentation.drawBox'
-- @
--
--   <<docs/gifs/doc_destroySprite.gif>>
destroySprite :: Sprite s -> Scene s ()
destroySprite (Sprite _ ref) = do
  now <- queryNow
  liftST $
    modifySTRef ref $ \(ttl, render) ->
      (if ttl < 0 then now else min ttl now, render)

-- | Low-level frame modifier.
spriteModify :: Sprite s -> Frame s ((SVG, ZIndex) -> (SVG, ZIndex)) -> Scene s ()
spriteModify (Sprite born ref) modFn = liftST $
  modifySTRef ref $ \(ttl, renderGen) ->
    ( ttl,
      do
        render <- renderGen
        modRender <- unFrame modFn
        return $ \relD relT ->
          let absT = relT + born in modRender absT relD relT . render relD relT
    )

-- | Map the SVG output of a sprite.
--
--   Example:
--
-- @
-- do s \<- 'fork' $ 'newSpriteA' 'Reanimate.Builtin.Documentation.drawCircle'
--    'wait' 1
--    'spriteMap' s 'Reanimate.Svg.Constructors.flipYAxis'
-- @
--
--   <<docs/gifs/doc_spriteMap.gif>>
spriteMap :: Sprite s -> (SVG -> SVG) -> Scene s ()
spriteMap sprite@(Sprite born _) fn = do
  now <- queryNow
  let tDelta = now - born
  spriteModify sprite $ do
    t <- spriteT
    return $ \(svg, zindex) -> (if (t - tDelta) < 0 then svg else fn svg, zindex)

-- | Modify the output of a sprite between @now@ and @now+duration@.
--
--   Example:
--
-- @
-- do s \<- 'fork' $ 'newSpriteA' 'Reanimate.Builtin.Documentation.drawCircle'
--    'spriteTween' s 1 $ \\val -> 'Reanimate.Svg.Constructors.translate' ('Reanimate.Constants.screenWidth'*0.3*val) 0
-- @
--
--   <<docs/gifs/doc_spriteTween.gif>>
spriteTween :: Sprite s -> Duration -> (Double -> SVG -> SVG) -> Scene s ()
spriteTween sprite@(Sprite born _) dur fn = do
  now <- queryNow
  let tDelta = now - born
  spriteModify sprite $ do
    t <- spriteT
    return $ first $ \svg -> fn (clamp 0 1 $ (t - tDelta) / dur) svg
  wait dur
  where
    clamp a b v
      | v < a = a
      | v > b = b
      | otherwise = v

-- | Create a new variable and apply it to a sprite.
--
--   Example:
--
-- @
-- do s \<- 'fork' $ 'newSpriteA' 'Reanimate.Builtin.Documentation.drawBox'
--    v \<- 'spriteVar' s 0 'Reanimate.Svg.Constructors.rotate'
--    'Reanimate.Scene.tweenVar' v 2 $ \\val -> 'Reanimate.fromToS' val 90
-- @
--
--   <<docs/gifs/doc_spriteVar.gif>>
spriteVar :: Sprite s -> a -> (a -> SVG -> SVG) -> Scene s (Var s a)
spriteVar sprite def fn = do
  v <- newVar def
  applyVar v sprite fn
  return v

-- | Apply an effect to a sprite.
--
--   Example:
--
-- @
-- do s <- 'fork' $ 'newSpriteA' 'Reanimate.Builtin.Documentation.drawCircle'
--    'spriteE' s $ 'Reanimate.Effect.overBeginning' 1 'Reanimate.Effect.fadeInE'
--    'spriteE' s $ 'Reanimate.Effect.overEnding' 0.5 'Reanimate.Effect.fadeOutE'
-- @
--
--   <<docs/gifs/doc_spriteE.gif>>
spriteE :: Sprite s -> Effect -> Scene s ()
spriteE (Sprite born ref) effect = do
  now <- queryNow
  liftST $
    modifySTRef ref $ \(ttl, renderGen) ->
      ( ttl,
        do
          render <- renderGen
          return $ \d t svg ->
            let (svg', z) = render d t svg
             in (delayE (max 0 $ now - born) effect d t svg', z)
      )

-- | Set new ZIndex of a sprite.
--
--   Example:
--
-- @
-- do s1 \<- 'newSpriteSVG' $ 'Reanimate.Svg.Constructors.withFillOpacity' 1 $ 'Reanimate.Svg.Constructors.withFillColor' "blue" $ 'Reanimate.Svg.Constructors.mkCircle' 3
--    'newSpriteSVG' $ 'Reanimate.Svg.Constructors.withFillOpacity' 1 $ 'Reanimate.Svg.Constructors.withFillColor' "red" $ 'Reanimate.Svg.Constructors.mkRect' 8 3
--    'wait' 1
--    'spriteZ' s1 1
--    'wait' 1
-- @
--
--   <<docs/gifs/doc_spriteZ.gif>>
spriteZ :: Sprite s -> ZIndex -> Scene s ()
spriteZ (Sprite born ref) zindex = do
  now <- queryNow
  liftST $
    modifySTRef ref $ \(ttl, renderGen) ->
      ( ttl,
        do
          render <- renderGen
          return $ \d t svg ->
            let (svg', z) = render d t svg in (svg', if t < now - born then z else zindex)
      )

-- | Destroy all local sprites at the end of a scene.
--
--   Example:
--
-- @
-- do -- the rect lives through the entire 3s animation
--    'newSpriteSVG_' $ 'Reanimate.Svg.Constructors.translate' (-3) 0 $ 'Reanimate.Svg.Constructors.mkRect' 4 4
--    'wait' 1
--    'spriteScope' $ do
--      -- the circle only lives for 1 second.
--      local \<- 'newSpriteSVG' $ 'Reanimate.Svg.Constructors.translate' 3 0 $ 'Reanimate.Svg.Constructors.mkCircle' 2
--      'spriteE' local $ 'Reanimate.Effect.overBeginning' 0.3 'Reanimate.Effect.fadeInE'
--      'spriteE' local $ 'Reanimate.Effect.overEnding' 0.3 'Reanimate.Effect.fadeOutE'
--      'wait' 1
--    'wait' 1
-- @
--
--   <<docs/gifs/doc_spriteScope.gif>>
spriteScope :: Scene s a -> Scene s a
spriteScope (M action) = M $ \t -> do
  (a, s, p, gens) <- action t
  return (a, s, p, map (genFn (t + max s p)) gens)
  where
    genFn maxT gen = do
      frameGen <- gen
      return $ \_ t ->
        if t < maxT
          then frameGen maxT t
          else (None, 0)

asAnimation :: (forall s'. Scene s' a) -> Scene s Animation
asAnimation s = do
  now <- queryNow
  return $ dropA now (scene (wait now >> s))

-- | Apply a transformation with a given overlap. This makes sure
--   to keep timestamps intact such that events can still be timed
--   by transcripts.
transitionO :: Transition -> Double -> (forall s'. Scene s' a) -> (forall s'. Scene s' b) -> Scene s ()
transitionO t o a b = do
  aA <- asAnimation a
  bA <- fork $ do
    wait (duration aA - o)
    asAnimation b
  play $ overlapT o t aA bA

#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Codec.Picture                   (PixelRGBA8 (PixelRGBA8))
import           Control.Arrow                   ((&&&))
import           Control.Lens                    ((.~))
import           Control.Monad                   (when)
import           Data.Foldable                   (forM_)
import qualified Data.Text                       as T
import           Reanimate
import           Reanimate.Builtin.Documentation (docEnv)
import           Reanimate.LaTeX                 (mathChunks)
import           Reanimate.Scene

import           Data.Bifoldable                 (Bifoldable (..), biforM_)
import           Data.Bifunctor                  (Bifunctor (..))
import           Data.Bitraversable              (Bitraversable (..))

data Expr op v
  = Var v
  | BinOp (Expr op v) op (Expr op v)
  | Paren op (Expr op v) op
  deriving (Show, Eq)

data Op = Add | Minus | LeftParen | RightParen deriving (Show, Eq)

oNewExpr :: Scene s (Expr (Op, Object s SVG) (T.Text, Object s SVG))
oNewExpr = bothAlongside (mapM (oNew . translate (-4) 0 . scale 2) . mathChunks)
         $ bimap (id &&& op2LaTeX) (id &&& id) expr
  where bothAlongside f = fmap runBiAlongside . fmap runBoth . f . Both . BiAlongside
        expr = Var "x" `minus` paren (Var "y" `add` Var "x")

main :: IO ()
main = reanimate $ docEnv $ mapA (withFillOpacity 1 . withStrokeWidth 0) $ scene $ do
  expr <- oNewExpr
  forM_ (Both $ BiAlongside expr) (`oShowWith` oDraw)
  let changeColor t = oContext .~ withFillColorPixel (PixelRGBA8 (round (t * 255)) 0 0 255)
  let changeColorRev t = changeColor (1 - t)
  waitOn $ biforM_ expr pure $ \(x, o) ->
    when (x == "x") $ fork $ do
      oTween o 1 changeColor
      oTween o 1 changeColorRev
  forM_ (Both $ BiAlongside expr) (fork . (`oHideWith` oFadeOut))

-- Some supportive functions.

add, minus :: Expr Op v -> Expr Op v -> Expr Op v
add x y = BinOp x Add y
minus x y = BinOp x Minus y

paren :: Expr Op v -> Expr Op v
paren x = Paren LeftParen x RightParen

op2LaTeX :: Op -> T.Text
op2LaTeX Add        = "+"
op2LaTeX Minus      = "-"
op2LaTeX LeftParen  = "("
op2LaTeX RightParen = ")"

-- Some supportive types and instances.

instance Bifunctor Expr where
  bimap _ g (Var x)        = Var (g x)
  bimap f g (BinOp x op y) = BinOp (bimap f g x) (f op) (bimap f g y)
  bimap f g (Paren l x r)  = Paren (f l) (bimap f g x) (f r)

instance Bifoldable Expr where
  bifoldMap _ g (Var x)        = g x
  bifoldMap f g (BinOp x op y) = bifoldMap f g x <> f op <> bifoldMap f g y
  bifoldMap f g (Paren l x r)  = f l <> bifoldMap f g x <> f r

instance Bitraversable Expr where
  bitraverse _ g (Var x)        = Var <$> g x
  bitraverse f g (BinOp x op y) = BinOp <$> bitraverse f g x <*> f op <*> bitraverse f g y
  bitraverse f g (Paren l x r)  = Paren <$> f l <*> bitraverse f g x <*> f r

-- | Given a bifunctor @f@, make a functor @Both f@ where @Both f a ~ f a a@.
newtype Both f a = Both { runBoth :: f a a }

instance Bifunctor p => Functor (Both p) where
  fmap f = Both . bimap f f . runBoth

instance Bifoldable p => Foldable (Both p) where
  foldMap f = bifoldMap f f . runBoth

instance Bitraversable p => Traversable (Both p) where
  traverse f = fmap Both . bitraverse f f . runBoth

-- | Make a bifunctor @f (a, -) (b, -)@ (namely @BiAlongside a b f@) out of @f@.
newtype BiAlongside a b f x y = BiAlongside { runBiAlongside :: f (a, x) (b, y) }

instance Bifunctor p => Bifunctor (BiAlongside a b p) where
  bimap f g = BiAlongside . bimap (second f) (second g) . runBiAlongside

instance Bifoldable p => Bifoldable (BiAlongside a b p) where
  bifoldMap f g = bifoldMap (f . snd) (g . snd) . runBiAlongside

instance Bitraversable p => Bitraversable (BiAlongside a b p) where
  bitraverse f g = fmap BiAlongside . bitraverse (traverse f) (traverse g) . runBiAlongside

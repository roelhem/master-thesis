module OptTh.Types.VanLaarhoven( Equality
                               , Equality'
                               , Fold
                               , Fold1
                               , Getter
                               , Iso
                               , Iso'
                               , Lens
                               , Lens'
                               , Prism
                               , Prism'
                               , Setter
                               , Setter'
                               , Traversal
                               , Traversal'
                               , Traversal1
                               , Traversal1'
                               , AffineTraversal
                               , AffineTraversal'
                               , affineTraversal
                               , AffineFold
                               , affineFold
                               , VLMap
                               , VLMap'
                               , CoVLMap
                               , CoVLMap'
                               , Grate
                               , Grate'
                               , grate
                               ) where

import Control.Lens
    ( Prism',
      Setter,
      Equality,
      Equality',
      Fold,
      Fold1,
      Getter,
      Iso,
      Iso',
      Lens,
      Lens',
      Prism,
      Setter',
      Traversal1,
      Traversal1' )
import Data.Functor.Apply (Apply)
import qualified OptTh.Types.Kinds as K
import Data.Either (either)
import Data.Maybe (maybe)
import Data.Pointed

type VLMap    c s t a b = forall f. c f => (a -> f b) -> s -> f t
type VLMap'   c s a     = VLMap c s s a a
type CoVLMap  c s t a b = forall f. c f => (f a -> b) -> f s -> t
type CoVLMap' c s a     = CoVLMap c s s a a

type Traversal  s t a b = forall f. (Apply f, Applicative f) => (a -> f b) -> s -> f t
type Traversal' s a     = Traversal s s a a

type PointedFunctor f = (Functor f, Pointed f)

-- | VanLaarhoven representation of an affine traversal.
-- | It can use @preview@, @over@, @set@ and @matching@
type AffineTraversal s t a b = VLMap PointedFunctor s t a b
-- | Same as @AffineTraversal@, but with @s ~ t@ and @a ~ b@.
type AffineTraversal' s a    = AffineTraversal s s a a

-- | Converts a simple affineTraversal morphism to the VanLaarhoven representation.
affineTraversal :: (s -> Either t (a, b -> t)) -> AffineTraversal s t a b
affineTraversal d f = either point (\(x,r) -> r <$> f x) . d

-- | VanLaarhoven representation of an affine fold.
type AffineFold s a = VLMap' PointedFunctor s a

-- | Converts a simple affineFold morphism to the VanLaarhoven representation.
affineFold :: (s -> Either s a) -> AffineFold s a
affineFold d f x = either point ((<$) x . f) $ d x

type Grate s t a b = CoVLMap Functor s t a b
type Grate' s a    = Grate s s a a

grate :: (((s -> a) -> b) -> t) -> Grate s t a b
grate d f x = d (f . (<$> x))
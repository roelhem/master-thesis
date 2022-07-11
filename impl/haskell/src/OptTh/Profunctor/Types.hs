module OptTh.Profunctor.Types where

import Data.Profunctor
import Data.Profunctor.Traversing
import qualified OptTh.Types.Kinds as K
import Data.Semigroup.Traversable (Traversable1)
import Data.Functor.Apply (Apply)
import Data.Pointed (Pointed)
import Data.Bifunctor (Bifunctor)

class Traversable f => Affine f where
  affine :: b -> (a -> b) -> f a -> b
  fromAffine :: a -> f a -> a

class (Choice p, Strong p) => AffineTraversing p where
  traverseAff' :: Affine f => p a b -> p (f a) (f b)
  wanderAff :: (forall f. Pointed f => (a -> f b) -> s -> f t) -> p a b -> p s t

class (Strong p) => Traversing1 p where
  traverse1' :: Traversable1 f => p a b -> p (f a) (f b)
  wander1 :: (forall f. Apply f => (a -> f b) -> s -> f t) -> p a b -> p s t

class Profunctor p => Classifying p where
  classify :: Applicative f => p a b -> p (f a) (f b)

class Bicontravariant p where
  contrabimap :: (b -> a) -> (d -> c) -> p a c -> p b d
  contrabimap f g = contrafirst f . contrasecond g
  contrafirst :: (b -> a) -> p a c -> p b c
  contrafirst = flip contrabimap id
  contrasecond :: (c -> b) -> p a b -> p a c
  contrasecond = contrabimap id
  {-# MINIMAL contrabimap|(contrafirst, contrasecond) #-}

type family Constraint k p where
  Constraint K.Iso p             = (Profunctor p)
  Constraint K.Lens p            = (Strong p)
  Constraint K.ReversedLens p    = (Costrong p)
  Constraint K.Prism p           = (Choice p)
  Constraint K.ReversedPrism p   = (Cochoice p)
  Constraint K.Grate p           = (Closed p)
  Constraint K.Glass p           = (Closed p, Strong p)
  Constraint K.Traversal p       = (Traversing p)
  Constraint K.Traversal1 p      = (Traversing1 p)
  Constraint K.AffineTraversal p = (AffineTraversing p)
  Constraint K.Setter p          = (Mapping p)
  Constraint K.Getter p          = (Bicontravariant p, Cochoice p, Strong p)
  Constraint K.AffineFold p      = (Bicontravariant p, Cochoice p, AffineTraversing p)
  Constraint K.Fold1 p           = (Bicontravariant p, Cochoice p, Traversing1 p)
  Constraint K.Fold p            = (Bicontravariant p, Cochoice p, Traversing p)
  Constraint K.Review p          = (Bifunctor p, Choice p, Costrong p)
  Constraint K.Kaleidoscope p    = (Classifying p)
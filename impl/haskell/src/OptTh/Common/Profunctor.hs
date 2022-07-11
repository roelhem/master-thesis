module OptTh.Common.Profunctor ( Profunctor
                               , Choice
                               , Cochoice
                               , Closed
                               , Mapping
                               , Costrong
                               , Strong
                               , Traversing
                               , AffineTraversing
                               , Traversing1
                               , Classifying
                               , Bicontravariant
                               , Bifunctor
                               ) where

import Prelude (flip, id, (.))
import OptTh.Common.Functor ( Apply
                            , Affine
                            , Pointed
                            , Traversable1
                            , Applicative
                            )
import Data.Profunctor.Traversing ( Traversing
                                  )
import Data.Profunctor ( Choice
                       , Profunctor
                       , Cochoice
                       , Closed
                       , Mapping
                       , Costrong
                       , Strong
                       , Profunctor
                       )
import Data.Bifunctor ( Bifunctor
                      )

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
module OptTh.Common.Functor ( Apply(..)
                            , Applicative(..)
                            , Distributive(..)
                            , Affine(..)
                            , Contravariant(..)
                            , Pointed(..)
                            , Copointed(..)
                            , Traversable(..)
                            , Traversable1(..)
                            , Foldable1(..)
                            , Settable(..)
                            ) where

import Data.Maybe (Maybe, maybe, fromMaybe)
import Control.Applicative (Applicative(..))
import Data.Traversable (Traversable(..))
import Data.Semigroup.Traversable (Traversable1(..))
import Data.Semigroup.Foldable (Foldable1(..))
import Data.Functor.Contravariant (Contravariant(..))
import Data.Distributive (Distributive(..))
import Data.Functor.Apply (Apply(..))
import Data.Pointed (Pointed(..))
import Data.Copointed (Copointed(..))
import Control.Lens (Settable(..))

class Traversable f => Affine f where
  affine :: b -> (a -> b) -> f a -> b
  fromAffine :: a -> f a -> a

instance Affine Maybe where
  affine = maybe
  fromAffine = fromMaybe
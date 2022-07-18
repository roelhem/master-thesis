module OptTh.Common.Functor ( Apply(..)
                            , Applicative(..)
                            , Distributive(..)
                            , Affine(..)
                            , Contravariant(..)
                            , Pointed(..)
                            , PointedF(..)
                            , Copointed(..)
                            , Traversable(..)
                            , Traversable1(..)
                            , Foldable(..)
                            , Foldable1(..)
                            , Settable(..)
                            , Functor(..)
                            , Identity(..)
                            , Compose(..)
                            , Default(..)
                            , AffineFoldable(..)
                            , Keep(..)
                            , After(..)
                            , Const(..)
                            , Θ1(..)
                            , copointFoldable1
                            , sequenceF
                            , type (⊚)
                            ) where

import Data.Functor (Functor(..))
import Data.Functor.Identity (Identity(..))
import Data.Maybe (Maybe (..), maybe, fromMaybe)
import Control.Applicative (Applicative(..), Const(..))
import Data.Traversable (Traversable(..), foldMapDefault)
import Data.Semigroup.Traversable (Traversable1(..), foldMap1Default, traverse1Maybe)
import Data.Semigroup.Foldable (Foldable1(..))
import Data.Functor.Contravariant (Contravariant(..))
import Data.Distributive (Distributive(..))
import Data.Functor.Apply (Apply(..), WrappedApplicative(..), (<.*>))
import Data.Functor.Compose(Compose(..))
import Data.Pointed (Pointed(..))
import Data.Copointed (Copointed(..))
import Control.Lens (Settable(..))
import GHC.Enum (Enum, Bounded)
import GHC.Base (Eq, Ord, Monoid, Semigroup, Constraint, NonEmpty, undefined)
import Data.Monoid (Monoid(..), First (..))
import Data.Semigroup (Semigroup(..), WrappedMonoid)
import Algebra.Lattice ( Lattice(..)
                       , BoundedMeetSemiLattice(..)
                       , BoundedJoinSemiLattice(..)
                       )
import Data.Data (Typeable, Data)
import GHC.Read (Read)
import GHC.Show (Show)
import GHC.Generics (Generic, Generic1)
import Algebra.PartialOrd (PartialOrd(..))
import Data.Bool (Bool(..))
import Algebra.Heyting (Heyting(..))
import Prelude (($), id, (<$>), (.))
import Data.Default (Default(..))
import Data.Foldable (Foldable (foldMap))
import Data.Tuple (curry)

-- Aliases

type (⊚) = Compose

-- Pointed

class (Functor f, Pointed f) => PointedF f where

-- Foldable

class Foldable t => AffineFoldable t where
  affine :: b -> (a -> b) -> t a -> b
  affine n m = maybe n m . toMaybe
  fromAffine :: a -> t a -> a
  fromAffine n = affine n id
  toMaybe :: t a -> Maybe a
  toMaybe = getFirst . foldMap point

-- Traverse

newtype Pre f x a = Pre { runPreApply ::  (a -> f x) -> x }

instance Functor     (Pre f x) where fmap f (Pre x)  = Pre (\g -> x (g . f))
instance Pointed f   => Apply   (Pre f x) where Pre f <.> Pre x = Pre (\g -> f ( \h -> point $ x (g . h)))
instance Copointed f => Pointed (Pre f x) where point a = Pre (\g -> copoint $ g a)
instance (Pointed f, Copointed f) => Applicative (Pre f x) where pure = point ; (<*>) = (<.>)

newtype UnsafePointed f a = UnsafePointed { runUnsafePointed :: f a } deriving (Eq, Ord, Functor)

instance Pointed f => Pointed (UnsafePointed f) where
  point x = UnsafePointed ( point x)

instance (Functor f, Pointed f) => Applicative (UnsafePointed f) where
  pure = point
  _ <*> _ = undefined

class (Traversable t, AffineFoldable t) => Affine t where
  traverseAff :: (Functor f, Pointed f) => (a -> f b) -> t a -> f (t b)
  traverseAff f = runUnsafePointed . traverse (UnsafePointed . f)
  sequenceP :: (Functor f, Pointed f) => t (f a) -> f (t a)
  sequenceP = traverseAff id

instance AffineFoldable Maybe where
  affine     = maybe
  fromAffine = fromMaybe
  toMaybe    = id


instance Affine Maybe where
  traverseAff _ Nothing = point Nothing
  traverseAff f (Just x) = Just <$> f x

instance AffineFoldable Identity where
  affine _ f x = f $ runIdentity x
  fromAffine _ = runIdentity

instance Affine Identity where
  traverseAff f (Identity x) = Identity <$> f x

-- Apply Helpers

newtype Θ1 f a = Θ1 { unΘ1 :: f a } deriving (Functor, Generic)

instance Applicative f => Apply (Θ1 f)       where Θ1 f <.> Θ1 x = Θ1 (f <*> x)
instance Applicative f => Pointed (Θ1 f)     where point x = Θ1 (pure x)
instance Applicative f => Applicative (Θ1 f) where pure = point ; (<*>) = (<.>)

newtype First1 a = First1 { getFirst1 :: a } deriving (Eq, Ord, Read, Show, Generic, Generic1, Functor)

instance Semigroup  (First1 a) where x <> _ = x
instance Pointed        First1 where point = First1
instance Copointed      First1 where copoint = getFirst1
instance Foldable       First1 where foldMap  f = f . copoint
instance Foldable1      First1 where foldMap1 f = f . copoint
instance AffineFoldable First1 where affine _ f = f . copoint
instance Traversable    First1 where sequenceA = fmap point . copoint
instance Traversable1   First1 where sequence1 = fmap point . copoint
instance Affine         First1 where sequenceP = fmap point . copoint
instance Apply          First1 where f <.> x = point (copoint f $ copoint x)

newtype Keep a b = Keep { runKeep :: (a, b) } deriving Functor

instance Apply (Keep a) where
  Keep (a, bf) <.> Keep (_, bx) = Keep (a, bf bx)

copointFoldable1 :: (Foldable1 t) => t a -> a
copointFoldable1 = getFirst1 . foldMap1 First1

sequenceF :: (Traversable1 t, Functor f) => t (f a) -> f (t a)
sequenceF tfa = (<$ tfa) <$> copointFoldable1 tfa

newtype After t a = After { runAfter :: (a, t a) } deriving Functor

instance Traversable t => Foldable  (After t) where foldMap  = foldMapDefault
instance Traversable t => Foldable1 (After t) where foldMap1 = foldMap1Default

instance Traversable t => Traversable (After t) where
  traverse f (After (a, as)) = curry After <$> f a <*> traverse f as

instance Traversable t => Traversable1 (After t) where
  traverse1 f (After (a, as)) = curry After <$> f a <.*> traverse1Maybe f as

-- Overlappables

instance {-# OVERLAPPABLE #-} (Default m, Semigroup m) => Monoid m where
  mempty = def

instance Monoid m => Default (WrappedMonoid m) where
  def = mempty

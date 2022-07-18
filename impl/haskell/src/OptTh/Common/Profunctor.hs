module OptTh.Common.Profunctor ( Profunctor(..)
                               , Choice(..)
                               , Cochoice(..)
                               , Closed(..)
                               , Mapping(..)
                               , Costrong(..)
                               , Strong(..)
                               , Traversing(..)
                               , Traversing1(..)
                               , Aggregating(..)
                               , Aggregating1(..)
                               , aggregating1Second'
                               , wanderAff
                               , traverseAff'
                               , wanderSum
                               , AffineAggregating(..)
                               , Bicontravariant(..)
                               , Bifunctor(..)
                               , EitherBazaar(..)
                               ) where

import Prelude (fst, snd, flip, id, (.), ($), (<$>), const, Foldable, Either(..), Monoid (..))
import OptTh.Common.Functor
import Data.Profunctor.Traversing ( Traversing(..)
                                  )
import Data.Profunctor ( Choice(..)
                       , Profunctor(..)
                       , Cochoice(..)
                       , Closed(..)
                       , Mapping(..)
                       , Costrong(..)
                       , Strong(..)
                       , Profunctor(..)
                       )
import Data.Bifunctor ( Bifunctor(..)
                      )
import Data.Kind (Constraint)
import Data.Foldable (Foldable(..))
import Data.Traversable (foldMapDefault)
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup (WrappedMonoid(..))
import Data.Functor.Apply (WrappedApplicative (..), MaybeApply(..))
import Data.Either (either)
import Control.Monad.State (State, state, evalState)
import Data.List (tail, head)
import OptTh.Common.Cardinalities
import GHC.Base (Symbol)
import Data.Semigroup.Traversable (foldMap1Default)

-- Convenience Types

-- BazLike

class TriFunctor (q :: * -> * -> * -> *) where
  tamap :: (a  -> a') -> q a b t -> q a' b  t
  tbmap :: (b' -> b ) -> q a b t -> q a  b' t
  ttmap :: (t  -> t') -> q a b t -> q a  b  t'
  tamap f = trimap f id id
  tbmap f = trimap id f id
  ttmap   = trimap id id
  trimap :: (a -> a') -> (b' -> b) -> (t -> t') -> q a b t -> q a' b' t'
  trimap f g h = tamap f . tbmap g . ttmap h
  {-# MINIMAL (tamap, tbmap, ttmap) | trimap #-}

class TriFunctor q => BazLike (q :: * -> * -> * -> *) where
  sell    :: a -> q a b b
  sold    :: q a a t -> t

  sellBaz :: a -> Baz q b b a
  sellBaz = Baz . sell
  soldBaz :: Baz q t a a -> t
  soldBaz = sold . unBaz
  sellFΘ :: Functor f => f a -> (Θ1 f ⊚ q a b) b
  sellFΘ = Compose . fmap sell . Θ1
  sellF :: Functor f => f a -> (f ⊚ q a b) b
  sellF = Compose . fmap sell

-- PostApp

instance (Profunctor p, Cardinality k) => TriFunctor (PreApp k p) where
  trimap f g h q = dimap g h $ mkPreApp (runPreApp q . lmap f)

instance (Cardinality k, AppLike k Identity) => BazLike (PreApp k (->)) where
  sell x = mkPreApp ($ x)
  sold x = runIdentity $ runPreApp x Identity

type PostApp (k :: Symbol) p = Baz (PreApp k p)

-- Baz

newtype Baz q t b a = Baz { unBaz :: q a b t }

instance TriFunctor q => Functor    (Baz q t b) where fmap   f     = Baz . tamap f . unBaz
instance TriFunctor q => Profunctor (Baz q t)   where dimap  f g   = Baz . tamap g . tbmap f . unBaz
instance TriFunctor q => TriFunctor (Baz q)     where trimap f g h = Baz . trimap h g f . unBaz

-- EitherBazaar

newtype EitherBazaar a b t = EitherBazaar { runEitherBazaar :: Either t (a, b -> t) }

instance Functor (EitherBazaar a b)  where fmap   f     (EitherBazaar e) = EitherBazaar $ bimap f ((second . fmap) f) e
instance Profunctor (EitherBazaar a) where dimap  f g   (EitherBazaar e) = EitherBazaar $ bimap g (second (dimap f g)) e
instance TriFunctor EitherBazaar     where trimap f g h (EitherBazaar e) = EitherBazaar $ bimap h (bimap f (dimap g h)) e

instance BazLike EitherBazaar where
  sell x = EitherBazaar (Right (x, id))
  sold = either id (\(a, f) -> f a) . runEitherBazaar

instance Pointed (EitherBazaar a b) where
  point = EitherBazaar . Left

-- instance TriFunctor q => Foldable (Baz q t b) where
--   foldMap f = _ . trimap (\a -> (a, a)) _ _ . unBaz

instance Foldable (Baz EitherBazaar t b) where foldMap = foldMapDefault

instance Traversable (Baz EitherBazaar t b) where
  traverse f (Baz (EitherBazaar (Right (a, bt)))) = Baz . EitherBazaar . Right . (,bt) <$> f a
  traverse f (Baz (EitherBazaar (Left t)))        = pure $ Baz (EitherBazaar (Left t))

instance AffineFoldable (Baz EitherBazaar t b)
instance Affine (Baz EitherBazaar t b) where
  traverseAff f (Baz (EitherBazaar (Right (a, bt)))) = Baz . EitherBazaar . Right . (,bt) <$> f a
  traverseAff f (Baz (EitherBazaar (Left t)))        = point $ Baz (EitherBazaar (Left t))

-- instance Cardinality k => Foldable (PostApp k (->) t b) where
--   foldMap = foldMapDefault

instance Foldable    (PostApp "*" (->) t b) where foldMap = foldMapDefault
instance Foldable    (PostApp "+" (->) t b) where foldMap = foldMapDefault
instance Foldable    (PostApp "!" (->) t b) where foldMap = foldMapDefault
instance Foldable    (PostApp "?" (->) t b) where foldMap = foldMapDefault
instance Traversable (PostApp "*" (->) t b) where sequenceA (Baz p) = unΘ1 $ Baz <$> getCompose (runPreApp p sellFΘ)
instance Traversable (PostApp "+" (->) t b) where sequenceA (Baz p) = unΘ1 $ Baz <$> getCompose (runPreApp p sellFΘ)
instance Traversable (PostApp "!" (->) t b) where sequenceA (Baz p) = unΘ1 $ Baz <$> getCompose (runPreApp p sellFΘ)
instance Traversable (PostApp "?" (->) t b) where sequenceA (Baz p) = unΘ1 $ Baz <$> getCompose (runPreApp p sellFΘ)

instance Foldable1    (PostApp "+" (->) t b) where foldMap1 = foldMap1Default
instance Foldable1    (PostApp "!" (->) t b) where foldMap1 = foldMap1Default
instance Traversable1 (PostApp "+" (->) t b) where sequence1 (Baz p) = Baz <$> getCompose (runPreApp p sellF)
instance Traversable1 (PostApp "!" (->) t b) where sequence1 (Baz p) = Baz <$> getCompose (runPreApp p sellF)

instance AffineFoldable  (PostApp "?" (->) t b)
instance AffineFoldable  (PostApp "!" (->) t b)
instance Affine (PostApp "?" (->) t b) where sequenceP (Baz p) = Baz <$> getCompose (runPreApp p sellF)
instance Affine (PostApp "!" (->) t b) where sequenceP (Baz p) = Baz <$> getCompose (runPreApp p sellF)

-- Traversals

wanderSum :: (Choice p, Strong p) => (s -> Either t (a, b -> t)) -> p a b -> p s t
wanderSum f = dimap f (either id (\(b, bt) -> bt b)) .  right' . first'

wanderAff :: (Choice p, Strong p) => (forall f. (Functor f, Pointed f) => (a -> f b) -> s -> f t) -> p a b -> p s t
wanderAff f = wanderSum (runEitherBazaar . f ( EitherBazaar . Right . (,id) ))

traverseAff' :: (Choice p, Strong p, Affine f) => p a b -> p (f a) (f b)
traverseAff' = wanderAff traverseAff

wanderer :: (Cardinality k, Profunctor p, AppLike k Identity) => (forall f. AppLike k f => (a -> f b) -> s -> f t) -> p (PostApp k (->) t b a) (PostApp k (->) t b b) -> p s t
wanderer f = dimap (\s -> Baz $ mkPreApp (`f` s)) soldBaz

class Strong p => Traversing1 p where
  traverse1' :: Traversable1 f => p a b -> p (f a) (f b)
  traverse1' = wander1 traverse1
  wander1 :: (forall f. Apply f => (a -> f b) -> s -> f t) -> p a b -> p s t
  wander1 f = wanderer @"+" f . traverse1'
  {-# MINIMAL traverse1' | wander1 #-}

-- traverse'' :: (Traversing1 p, Choice p, Traversable f) => p a b -> p (f a) (f b)
-- traverse'' = dimap _ _ . traverse1' . left'

-- wander' :: (Traversing1 p, Choice p) => (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t
-- wander' f = dimap _ _ . left' . traverse1'

-- Aggregating

aggregator :: (Cardinality k, Profunctor p, AppLike k Identity, TravLike k (Baz (PreApp k (->)) b a)) => (forall f. TravLike k f => (f a -> b) -> f s -> t) -> p (PreApp k (->) s a a) (PreApp k (->) s a b) -> p s t
aggregator f = dimap sell (f soldBaz . Baz)

class Closed p => Aggregating p where
  aggregate' :: Applicative f => p a b -> p (f a) (f b)
  aggregate' = aggregating ((. sequenceA) . fmap)
  aggregating :: (forall f. Traversable f => (f a -> b) -> f s -> t) -> p a b -> p s t
  aggregating f = aggregator @"*" f . aggregate'
  {-# MINIMAL aggregate' | aggregating #-}

-- aggregate1'' :: (Aggregating p, Strong p) => Apply f => p a b -> p (f a) (f b)
-- aggregate1'' = aggregating _ . second'

aggregating1' :: (Strong p, Aggregating p) => (forall f. Traversable1 f => (f a -> b) -> f s -> t) -> p a b -> p s t
aggregating1' g = dimap sell' (back g) . second' . aggregate' where
  sell' :: s -> (s, PreApp "*" (->) s a a)
  sell' s = (s, sell s)
  back :: ((After (PostApp "*" (->) b a) a -> b) -> After (PostApp "*" (->) b a) s -> t) -> (s, PreApp "*" (->) s a b) -> t
  back g (s, ss) = g (soldBaz . snd . runAfter) (After (s, Baz ss))

-- After (PostApp "*" (->) s a) a

class (Strong p, Aggregating p) => Aggregating1 p where
  aggregate1' :: Apply f => p a b -> p (f a) (f b)
  aggregate1' = aggregating1 ((. sequence1) . fmap)
  aggregating1 :: (forall f. Traversable1 f => (f a -> b) -> f s -> t) -> p a b -> p s t
  aggregating1 f = aggregator @"+" f . aggregate1'
  {-# MINIMAL aggregate1' | aggregating1 #-}

class (Choice p, Aggregating p) => AffineAggregating p where
  aggregateAff' :: (Functor f, Pointed f) => p a b -> p (f a) (f b)
  aggregateAff' = aggregatingAff ((. sequenceP) . fmap)
  aggregatingAff :: (forall f. Affine f => (f a -> b) -> f s -> t) -> p a b -> p s t
  aggregatingAff f = aggregator @"?" f . aggregateAff'
  {-# MINIMAL aggregateAff' | aggregatingAff #-}

aggregating1Second' :: Aggregating1 p => p a b -> p (c, a) (c, b)
aggregating1Second' = dimap Keep runKeep . aggregate1'

-- Bicontravariant

class Bicontravariant p where
  contrabimap :: (b -> a) -> (d -> c) -> p a c -> p b d
  contrabimap f g = contrafirst f . contrasecond g
  contrafirst :: (b -> a) -> p a c -> p b c
  contrafirst = flip contrabimap id
  contrasecond :: (c -> b) -> p a b -> p a c
  contrasecond = contrabimap id
  {-# MINIMAL contrabimap|(contrafirst, contrasecond) #-}

-- Folders

-- Helpers

instance {-# OVERLAPPABLE #-} (Profunctor p, Bicontravariant p) => Strong p where
  first'   = contrabimap fst fst
  second'  = contrabimap snd snd

instance {-# OVERLAPPABLE #-} (Profunctor p, Bicontravariant p) => Cochoice p where
  unleft   = contrabimap Left  Left
  unright  = contrabimap Right Right

instance {-# OVERLAPPABLE #-} (Profunctor p, Bifunctor p) => Costrong p where
  unfirst  = bimap fst fst
  unsecond = bimap snd snd

instance {-# OVERLAPPABLE #-} (Profunctor p, Bifunctor p) => Choice p where
  left'    = bimap Left  Left
  right'   = bimap Right Right

instance {-# OVERLAPPABLE #-} (Profunctor p, Bifunctor p) => Closed p where
  closed = bimap const const

instance {-# OVERLAPPABLE #-} (Profunctor p, Bifunctor p) => Aggregating p where
  aggregate' = bimap pure pure
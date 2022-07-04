{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module OptTh.Simple.Helpers where

import OptTh.Simple.Types
import Data.Functor.Apply (Apply, MaybeApply(..))
import Data.Semigroup (Endo(..), Dual(..), Sum (..), Product (..))
import GHC.Base (build, NonEmpty (..))

----------------------------------------------------------------------------
---------------------- SIMPLE OPTIC GADT DEFINITIONS -----------------------
----------------------------------------------------------------------------

-- Getter --
get :: (o ~> Getter) => o s t a b -> (s -> a)
get o = case og @_ @Getter o of Getter f -> f

-- Setter --
over :: (o ~> Setter) => o s t a b -> ((a -> b) -> (s -> t))
over o = case og @_ @Setter o of Setter f -> f

put :: (o ~> Setter) => o s t a b -> (s -> b -> t)
put o s b = over o (const b) s

set :: (o ~> Setter) => o s t a b -> (b -> s -> t)
set o b = over o (const b)

-- AffineTraversal --
matching :: (o ~> AffineTraversal) => o s t a b -> (s -> Either t a)
matching o = case og @_ @AffineTraversal o of AffineTraversal f -> fmap fst <$> f

-- Traversal --
traversing :: (o ~> Traversal) => o s t a b -> (forall f. (Apply f, Applicative f) => (a -> f b) -> s -> f t)
traversing o = case og @_ @Traversal o of Traversal f -> f

travMaybe :: (o ~> Traversal) => o s t a b -> (forall f. Apply f => (a -> f b) -> s -> MaybeApply f t)
travMaybe o f = traversing o (MaybeApply . Left . f)

-- Traversal1 --
traversing1 :: (o ~> Traversal1) => o s t a b -> (forall f. Apply f => (a -> f b) -> s -> f t)
traversing1 o = case og @_ @Traversal1 o of Traversal1 f -> f

-- AffineFold --
preview :: (o ~> AffineFold) => o s t a b -> (s -> Maybe a)
preview o = case og @_ @AffineFold o of AffineFold f -> f

-- Fold --
foldMapOf :: (o ~> Fold) => o s t a b -> (forall m. Monoid m => (a -> m) -> s -> m)
foldMapOf o = case og @_ @Fold o of Fold f -> f

toListOf :: (o ~> Fold) => o s t a b -> s -> [a]
toListOf o s = build (\f n -> foldrOf o f n s)

foldrOf :: (o ~> Fold) => o s t a b -> (a -> x -> x) -> x -> s -> x
foldrOf o f x s = appEndo (foldMapOf o (Endo . f) s) x

foldlOf :: (o ~> Fold) => o s t a b -> (x -> a -> x) -> x -> s -> x
foldlOf o f x s = appEndo (getDual (foldMapOf o (Dual . Endo . flip f) s)) x

foldOf :: (o ~> Fold, Monoid a) => o s t a b -> s -> a
foldOf o = foldMapOf o id

lengthOf :: (o ~> Fold) => o s t a b -> s -> Int
lengthOf o = length . toListOf o

sumOf :: (o ~> Fold, Num a) => o s t a b -> s -> a
sumOf o = getSum . foldMapOf o Sum

productOf :: (o ~> Fold, Num a) => o s t a b -> s -> a
productOf o = getProduct . foldMapOf o Product

-- Fold1 --
fold1MapOf :: (o ~> Fold1) => o s t a b -> (forall m. Semigroup m => (a -> m) -> s -> m)
fold1MapOf o = case og @_ @Fold1 o of Fold1 f -> f

toNonEmptyOf :: (o ~> Fold1) => o s t a b -> s -> NonEmpty a
toNonEmptyOf o = fold1MapOf o (:|[])

fold1Of :: (o ~> Fold1, Semigroup a) => o s t a b -> s -> a
fold1Of o = fold1MapOf o id

-- Kaleidoscope --
aggregate :: (o ~> Kaleidoscope) => o s t a b -> (([a] -> b) -> [s] -> t)
aggregate o = case og @_ @Kaleidoscope o of Kaleidoscope f -> f

-- Review --
review :: (o ~> Review) => o s t a b -> (b -> t)
review o = case og @_ @Review o of Review f -> f

----------------------------------------------------------------------------
---------------------- CONCATINATION OF TRAVERSALS -------------------------
----------------------------------------------------------------------------
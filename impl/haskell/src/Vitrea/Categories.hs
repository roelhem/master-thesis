{-|
Module      : Categories
Description : Constrained categories
Copyright   : (c) Mario Román, 2020
License     : GPL-3
Maintainer  : mromang08@gmail.com
Stability   : experimental
Portability : POSIX
Provides a definition of category enriched over Hask, where the sets of objects
are represented by constraints.  Considers also functors and monoidal categories.
-}

module Vitrea.Categories where

import Data.Functor.Compose
import Data.Functor.Identity
import Data.Void
import Control.Monad

-- | Definition of a category enriched over the language. The sets of objects
-- are represented by constraints.
class Category objc c where
  unit :: (objc x) => c x x
  comp :: (objc x) => c y z -> c x y -> c x z

-- | Functors.
class ( Category objc c, Category objd d
      , forall x . objc x => objd (f x)
      ) => VFunctor objc c objd d f where
  map :: (objc x, objc y) => c x y -> d (f x) (f y)

-- | Bifunctors.
class ( Category objc c, Category objd d, Category obje e
      , forall x y . (objc x , objd y) => obje (f x y) )
      => Bifunctor objc c objd d obje e f where
  bimap :: ( objc x1, objc x2, objd y1, objd y2 )
        => c x1 x2 -> d y1 y2 -> e (f x1 y1) (f x2 y2)

-- | Profunctors.
class ( Category objc c, Category objd d )
      => Profunctor objc c objd d p where
  dimap :: (objc x1, objc x2, objd y1, objd y2)
        => c x2 x1 -> d y1 y2 -> p x1 y1 -> p x2 y2

-- | Monoidal categories. The definition follows that of an enriched monoidal
-- category, taking the language as the base of enrichment.
class ( Category obja a
      , Bifunctor obja a obja a obja a o
      , obja i )
      => MonoidalCategory obja a o i where
  alpha  :: (obja x, obja y, obja z)
         => a (x `o` (y `o` z)) ((x `o` y) `o` z)
  alphainv  :: (obja x, obja y, obja z)
            => a ((x `o` y) `o` z) (x `o` (y `o` z))
  lambda    :: (obja x) => a (x `o` i) x
  lambdainv :: (obja x) => a x (x `o` i)
  rho       :: (obja x) => a (i `o` x) x
  rhoinv    :: (obja x) => a x (i `o` x)

-- | Monoidal actions as suitable bifunctors with the corresponding structure
-- maps.
class ( MonoidalCategory objm m o i
      , Bifunctor objm m objc c objc c f
      , Category objc c )
      => MonoidalAction objm m o i objc c f where
  unitor :: (objc x) => c (f i x) x
  unitorinv :: (objc x) => c x (f i x)
  multiplicator :: (objc x, objm p, objm q)
                => c (f p (f q x)) (f (p `o` q) x)
  multiplicatorinv :: (objc x, objm p, objm q)
                => c (f (p `o` q) x) (f p (f q x))


-- Instances for some common categories
class Any x
instance Any x

instance Category Any (->) where
  comp g f x = g (f x)
  unit x = x

newtype Nat f g = Nat { nat :: forall x . f x -> g x }

instance Category Functor Nat where
  unit = Nat id
  comp (Nat h) (Nat k) = Nat (h . k)
instance Category Applicative Nat where
  unit = unit @Functor
  comp = comp @Functor
instance Category Traversable Nat where
  unit = unit @Functor
  comp = comp @Functor

newtype App f a = App { getApp :: f a }

instance Bifunctor Any (->) Any (->) Any (->) (,) where
  bimap f g (x,y) = (f x , g y)

instance Bifunctor Functor Nat Functor Nat Functor Nat Compose where
  bimap (Nat h) (Nat k) = Nat (Compose . fmap k . h . getCompose)
instance Bifunctor Applicative Nat Applicative Nat Applicative Nat Compose where
  bimap = bimap @Functor @Nat @Functor @Nat @Functor @Nat @Compose
instance Bifunctor Traversable Nat Traversable Nat Traversable Nat Compose where
  bimap = bimap @Functor @Nat @Functor @Nat @Functor @Nat @Compose


instance Bifunctor Any (->) Any (->) Any (->) Either where
  bimap f _ (Left x)  = Left  (f x)
  bimap _ g (Right x) = Right (g x)

instance Bifunctor Functor Nat Any (->) Any (->) App where
  bimap (Nat h) f = App . fmap f . h . getApp
instance Bifunctor Applicative Nat Any (->) Any (->) App where
  bimap = bimap @Functor @Nat @Any @(->) @Any @(->) @App
instance Bifunctor Traversable Nat Any (->) Any (->) App where
  bimap = bimap @Functor @Nat @Any @(->) @Any @(->) @App


instance MonoidalCategory Any (->) (,) () where
  alpha (x,(y,z)) = ((x,y),z)
  alphainv ((x,y),z) = (x,(y,z))
  lambda (x,_) = x
  lambdainv x = (x,())
  rho (_,x) = x
  rhoinv x = ((),x)

instance MonoidalCategory Any (->) Either Void where
  alpha = either (Left . Left) (either (Left . Right) Right)
  alphainv = either (fmap Left) (Right . Right)
  lambda = either id absurd
  lambdainv = Left
  rho = either absurd id
  rhoinv = Right

instance MonoidalCategory Functor Nat Compose Identity where
  alpha = Nat (Compose . Compose . fmap getCompose . getCompose)
  alphainv = Nat (Compose . fmap Compose . getCompose . getCompose)
  lambda = Nat (fmap runIdentity . getCompose)
  lambdainv = Nat (Compose . fmap Identity)
  rho = Nat (runIdentity . getCompose)
  rhoinv = Nat (Compose . Identity)

instance MonoidalCategory Applicative Nat Compose Identity where
  alpha = alpha @Functor @Nat @Compose @Identity
  alphainv = alphainv @Functor @Nat @Compose @Identity
  lambda = lambda @Functor @Nat @Compose @Identity
  lambdainv = lambdainv @Functor @Nat @Compose @Identity
  rho = rho @Functor @Nat @Compose @Identity
  rhoinv = rhoinv @Functor @Nat @Compose @Identity

instance MonoidalCategory Traversable Nat Compose Identity where
  alpha = alpha @Functor @Nat @Compose @Identity
  alphainv = alphainv @Functor @Nat @Compose @Identity
  lambda = lambda @Functor @Nat @Compose @Identity
  lambdainv = lambdainv @Functor @Nat @Compose @Identity
  rho = rho @Functor @Nat @Compose @Identity
  rhoinv = rhoinv @Functor @Nat @Compose @Identity

instance MonoidalAction Functor Nat Compose Identity Any (->) App where
  unitor = runIdentity . getApp
  unitorinv = App . Identity
  multiplicator = App . Compose . fmap getApp . getApp
  multiplicatorinv = App . fmap App . getCompose . getApp

instance MonoidalAction Applicative Nat Compose Identity Any (->) App where
  unitor = unitor @Functor @Nat @Compose @Identity @Any @(->) @App
  unitorinv = unitorinv @Functor @Nat @Compose @Identity @Any @(->) @App
  multiplicator = multiplicator @Functor @Nat @Compose @Identity @Any @(->) @App
  multiplicatorinv = multiplicatorinv @Functor @Nat @Compose @Identity @Any @(->) @App

instance MonoidalAction Traversable Nat Compose Identity Any (->) App where
  unitor = unitor @Functor @Nat @Compose @Identity @Any @(->) @App
  unitorinv = unitorinv @Functor @Nat @Compose @Identity @Any @(->) @App
  multiplicator = multiplicator @Functor @Nat @Compose @Identity @Any @(->) @App
  multiplicatorinv = multiplicatorinv @Functor @Nat @Compose @Identity @Any @(->) @App

instance (MonoidalCategory objm m o i) => MonoidalAction objm m o i objm m o where
  unitor = rho @objm @m
  unitorinv = rhoinv @objm @m
  multiplicator = alpha @objm @m @o @i
  multiplicatorinv = alphainv @objm @m @o @i


class (Monad m) => Algebra m a where
  algebra :: m a -> a

instance (Monad m) => Algebra m (m a) where
  algebra = join

instance (Monad m) => Category (Algebra m) (->) where
  unit = unit @Any
  comp = comp @Any

instance (Monad m) => Bifunctor (Algebra m) (->) (Algebra m) (->) (Algebra m) (->) (,) where
  bimap = bimap @Any @(->) @Any @(->) @Any @(->)

instance (Monad m) => Algebra m () where
  algebra _ = ()

instance (Monad m, Algebra m x, Algebra m y) => Algebra m (x,y) where
  algebra u = (algebra $ fmap fst u, algebra $ fmap snd u)

instance (Monad m) => MonoidalCategory (Algebra m) (->) (,) () where
  alpha     = alpha     @Any @(->) @(,) @()
  alphainv  = alphainv  @Any @(->) @(,) @()
  lambda    = lambda    @Any @(->) @(,) @()
  lambdainv = lambdainv @Any @(->) @(,) @()
  rho       = rho       @Any @(->) @(,) @()
  rhoinv    = rhoinv    @Any @(->) @(,) @()

instance (Monad m) => Bifunctor (Algebra m) (->) Any (->) Any (->) (,) where
  bimap = bimap @Any @(->) @Any @(->) @Any @(->) @(,)

instance (Monad m) => MonoidalAction (Algebra m) (->) (,) () Any (->) (,) where
  unitor           = unitor           @Any @(->) @(,) @() @Any @(->) @(,)
  unitorinv        = unitorinv        @Any @(->) @(,) @() @Any @(->) @(,)
  multiplicator    = multiplicator    @Any @(->) @(,) @() @Any @(->) @(,)
  multiplicatorinv = multiplicatorinv @Any @(->) @(,) @() @Any @(->) @(,)

-- FunLists are the free applicatives over Store functors. We
-- implement the type-changing FunList.
data FunList s a b = Done b | More s (FunList s a (a -> b))

instance Functor (FunList s a) where
  fmap f (Done b) = Done (f b)
  fmap f (More s l) = More s (fmap (f .) l)

-- This instance declaration can be seen, for instance, in Pickering,
-- Gibbons, Wu.
instance Applicative (FunList s a) where
  pure = Done
  (Done f) <*> l' = fmap f l'
  (More x l) <*> l' = More x (fmap flip l <*> l')

-- Kleisli categories of free algebras.  The morphisms Kl(a,b) are
-- written in the usual (a -> m b) form.
newtype Kleisli m a b = Kleisli { getKleisli :: a -> m b }

instance (Monad m) => Category Any (Kleisli m) where
  unit = Kleisli return
  comp (Kleisli g) (Kleisli f) = Kleisli (g <=< f)

instance (Monad m) => Bifunctor Any (->) Any (Kleisli m) Any (Kleisli m) (,) where
  bimap f (Kleisli g) = Kleisli (\(x,y) -> fmap (f x,) (g y))

instance (Monad m) => MonoidalAction Any (->) (,) () Any (Kleisli m) (,) where
  unitor = unitor @Any @(->) @(,) @() @Any @(Kleisli m) @(,)
  unitorinv = unitorinv @Any @(->) @(,) @() @Any @(Kleisli m) @(,)
  multiplicator = multiplicator @Any @(->) @(,) @() @Any @(Kleisli m) @(,)
  multiplicatorinv = multiplicatorinv @Any @(->) @(,) @() @Any @(Kleisli m) @(,)
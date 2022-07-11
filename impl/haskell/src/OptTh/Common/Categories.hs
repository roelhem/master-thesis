module OptTh.Common.Categories where

import OptTh.Common.Constraints
import Prelude
import Data.Kind (Type, Constraint)

infixr 9 `comp`

------------------------------------------------------------------------------------------
---------------------------------- BASIC CATEGORY ----------------------------------------
------------------------------------------------------------------------------------------

class Category objc c where
  unit :: (objc x) => c x x
  comp :: (objc x) => c y z -> c x y -> c x z

instance Category Any (->) where
  g `comp` f = g . f
  unit       = id

class Category' (k :: * -> * -> Type) where
  type Object k (o :: *) :: Constraint
  unit' :: (Object k a) => a `k` a
  comp' :: (Object k a) => b `k` c -> a `k` b -> a `k` c

------------------------------------------------------------------------------------------
---------------------- FUNCTORS AND NATURAL TRANSFORMATIONS ------------------------------
------------------------------------------------------------------------------------------

newtype (:=>) f g = Nat { natTo :: forall x. f x -> g x }

instance Category Functor (:=>) where
  unit = Nat id
  comp (Nat h) (Nat k) = Nat (h . k)
instance Category Applicative (:=>) where
  unit = unit @Functor
  comp = comp @Functor
instance Category Traversable (:=>) where
  unit = unit @Functor
  comp = comp @Functor

------------------------------------------------------------------------------------------
-------------------------------- OPTICS CATEGORIES ---------------------------------------
------------------------------------------------------------------------------------------

infixr 9 %

class f ~> g where
  oTo :: f s t a b -> g s t a b

instance f ~> f where
  oTo = id

class Composable' f g h where
  (%?) :: f s t x y -> g x y a b -> h s t a b

class Composable' f g h => Composable f g h | f g -> h where
  (%) :: f s t x y -> g x y a b -> h s t a b
  (%) = (%?)

instance {-# OVERLAPPABLE #-} (o0 ~> o2, o1 ~> o2, Composable' o2 o2 o2) => Composable' o0 o1 o2 where
  f %? g = oTo @o0 @o2 f %? oTo @o1 @o2 g
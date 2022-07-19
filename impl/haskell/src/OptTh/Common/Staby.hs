module OptTh.Common.Staby where

import Prelude ((.), Functor, id)
import OptTh.Common.Profunctor
import OptTh.Common.Categories
import OptTh.Common.Functor

----------------------------------------------------------------------------
---------------------------- STABY TYPECLASS -------------------------------
----------------------------------------------------------------------------

class Staby (q :: * -> * -> * -> * -> *) where
  smap :: (s' -> s ) -> q  s t a b -> q  s' t  a  b
  smap f = stabmap f id id id
  tmap :: (t  -> t') -> q  s t a b -> q  s  t' a  b
  tmap f = stabmap id f id id
  amap :: (a  -> a') -> q  s t a b -> q  s  t  a' b
  amap f = stabmap id id f id
  bmap :: (b' -> b ) -> q  s t a b -> q  s  t  a  b'
  bmap = stabmap id id id
  stmap :: (s' -> s) -> (t -> t') -> q s t a b -> q s' t' a  b
  stmap fs ft = tmap ft . smap fs
  abmap :: (a -> a') -> (b' -> b) -> q s t a b -> q s  t  a' b'
  abmap fa fb = bmap fb . amap fa
  samap :: (s' -> s) -> (a -> a') -> q s t a b -> q s' t  a' b
  samap fs fa = amap fa . smap fs
  tbmap :: (t -> t') -> (b' -> b) -> q s t a b -> q s  t' a  b'
  tbmap ft fb = bmap fb . tmap ft
  stabmap :: (s' -> s) -> (t -> t') -> (a -> a') -> (b' -> b) -> q s t a b -> q s' t' a' b'
  stabmap fs ft fa fb = bmap fb . amap fa . tmap ft . smap fs
  {-# MINIMAL (smap, tmap, amap, bmap) | stabmap #-}

----------------------------------------------------------------------------
--------------------------------- STAB -------------------------------------
----------------------------------------------------------------------------

newtype STAB (q :: * -> * -> * -> * -> *) s t a b = STAB { unSTAB :: q s t a b }

instance Composable f g h => Composable (STAB f) (STAB g) (STAB h) where
  STAB f % STAB g = STAB (f % g)

instance ProCategory q => ProCategory (STAB q) where
  id2 = STAB id2

instance Staby q => Staby (STAB q) where
  smap f (STAB x) = STAB (smap f x)
  tmap f (STAB x) = STAB (tmap f x)
  amap f (STAB x) = STAB (amap f x)
  bmap f (STAB x) = STAB (bmap f x)

instance Staby q => Contravariant (STAB q s t a) where
  contramap f (STAB x) = STAB (bmap f x)

----------------------------------------------------------------------------
--------------------------------- TABS -------------------------------------
----------------------------------------------------------------------------

newtype TABS (q :: * -> * -> * -> * -> *) t a b s = TABS { unTABS :: q s t a b }

instance Staby q => Contravariant (TABS q t a b) where
  contramap f (TABS x) = TABS (smap f x)

----------------------------------------------------------------------------
--------------------------------- SABT -------------------------------------
----------------------------------------------------------------------------

newtype SABT (q :: * -> * -> * -> * -> *) s a b t = SABT { unSABT :: q s t a b }

instance Staby q => Functor (SABT q s a b) where
  fmap f (SABT x) = SABT (tmap f x)

instance Staby q => Profunctor (SABT q s a) where
  dimap f g (SABT x) = SABT (tbmap g f x)

----------------------------------------------------------------------------
--------------------------------- STBA -------------------------------------
----------------------------------------------------------------------------

newtype STBA (q :: * -> * -> * -> * -> *) s t b a = STBA { unSTBA :: q s t a b }

instance Staby q => Functor (STBA q s t b) where
  fmap f (STBA x) = STBA (amap f x)

instance Staby q => Profunctor (STBA q s t) where
  dimap f g (STBA x) = STBA (abmap g f x)

----------------------------------------------------------------------------
--------------------------------- TSBA -------------------------------------
----------------------------------------------------------------------------

newtype TSBA (q :: * -> * -> * -> * -> *) t s b a = TSBA { unTSBA :: q s t a b }

instance Composable f g h => Composable (TSBA f) (TSBA g) (TSBA h) where
  TSBA g % TSBA f = TSBA (g % f)

instance ProCategory o => ProCategory (TSBA o) where
  id2 = TSBA id2

----------------------------------------------------------------------------
--------------------------------- ABST -------------------------------------
----------------------------------------------------------------------------

newtype ABST (q :: * -> * -> * -> * -> *) a b s t = ABST { unABST :: q s t a b }

instance Composable f g h => Composable (ABST g) (ABST f) (ABST h) where
  ABST g % ABST f = ABST (f % g)

instance ProCategory o => ProCategory (ABST o) where
  id2 = ABST id2

instance Staby q => Functor (ABST q s t b) where
  fmap f (ABST x) = ABST (tmap f x)

instance Staby q => Profunctor (ABST q s t) where
  dimap f g (ABST x) = ABST (stmap f g x)

----------------------------------------------------------------------------
--------------------------------- BATS -------------------------------------
----------------------------------------------------------------------------

newtype BATS (q :: * -> * -> * -> * -> *) b a t s = BATS { unBATS :: q s t a b }

instance Composable f g h => Composable (BATS g) (BATS f) (BATS h) where
  BATS g % BATS f = BATS (f % g)

instance ProCategory q => ProCategory (BATS q) where
  id2 = BATS id2

instance Staby q => Contravariant (BATS q b a t) where
  contramap f (BATS x) = BATS (smap f x)

instance Staby q => Staby (BATS q) where
  stabmap fs ft fa fb (BATS x) = BATS (stabmap fb fa ft fs x)
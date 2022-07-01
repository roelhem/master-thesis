{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
module OptTh.Types.Extra where

import Control.Applicative
import Data.Complex (Complex)
import Control.Lens (Identity, Contravariant, Lens)
import Data.Semigroup
import Data.List.NonEmpty (NonEmpty)
import Control.Comonad (Comonad (extract))

-- Constraint union, a type-level monoid.
infixl 7 :&:
class    (c a, d a) => (c :&: d) a
instance (c a, d a) => (c :&: d) a
class    CEmpty a
instance CEmpty a

-- DataFunctor
newtype DT t v = DT { dtFmap :: forall w . (v -> w) -> t w }

dtExtract :: DT t v -> t v
dtExtract (DT g) = g id

mkDT :: Functor f => f v -> DT f v
mkDT x = DT (`fmap` x)

instance Functor (DT t) where
    fmap f (DT g) = DT (\h -> g (h . f))

-- Units
-- | Functors for which there exists a monomorphic natural transformation ID => Functor
class Functor f => Unit f where
    upure :: a -> f a
    default upure :: Applicative f => a -> f a
    upure = pure

instance (Functor a, Applicative a) => Unit a

class    (Contravariant f, Unit f) => ContraUnit f
instance (Contravariant f, Unit f) => ContraUnit f

----------------------------------------------------------------------------
----------------------------- APPLY ----------------------------------------
----------------------------------------------------------------------------

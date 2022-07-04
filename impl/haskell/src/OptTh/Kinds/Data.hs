{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module OptTh.Kinds.Data where

import Data.Functor.Identity (Identity)
import Data.Data (Proxy)
import Algebra.PartialOrd (PartialOrd)
import Data.Bifunctor
import GHC.Generics (Generic)

type OpticKind' = OpticKind Identity

data OpticKind m where
  Equality        :: OpticKind'
  Iso             :: OpticKind'
  Lens            :: OpticKind'
  AlgLens         :: Monad m => Proxy m -> OpticKind m
  Prism           :: OpticKind'
  AlgPrism        :: Monad m => Proxy m -> OpticKind m
  AffineTraversal :: OpticKind'
  Setter          :: OpticKind'
  Unknown         :: OpticKind'

deriving instance Eq (OpticKind m)

data OpticProxy (l :: * -> * -> *) (r :: * -> * -> *) = OpticProxy { leftAction :: Proxy l 
                                                                   , rightAction :: Proxy r
                                                                   } deriving (Show, Eq)

----------------------------------------------------------------------------
--------------------------- TENSOR PRODUCTS --------------------------------
----------------------------------------------------------------------------

class Bifunctor f => AssociativeTensor (f :: * -> * -> *) where
  associator :: f (f a b) c -> f a (f b c)
  coassociator :: f a (f b c) -> f (f a b) c

class (AssociativeTensor f) => MonoidalTensor (f :: * -> * -> *) where
  type Unit f :: *
  unitorLeft     :: f (Unit f) b -> b
  counitorLeft   :: b -> f (Unit f) b
  unitorRight    :: f a (Unit f) -> a
  counitorRight  :: a -> f a (Unit f)

instance AssociativeTensor (,) where
  associator ((a,b),c) = (a,(b,c))
  coassociator (a,(b,c)) = ((a,b),c)

instance MonoidalTensor (,) where
  type Unit (,) = ()
  unitorLeft ((), b) = b
  counitorLeft b = ((), b)
  unitorRight (a, ()) = a
  counitorRight a = (a, ())

instance AssociativeTensor Either where
  associator (Left (Left a)) = Left a
  associator (Left (Right b)) = Right (Left b)
  associator (Right c) = Right (Right c)
  coassociator (Left a) = Left (Left a)
  coassociator (Right (Left b)) = Left (Right b)
  coassociator (Right (Right c)) = Right c

instance MonoidalTensor Either where
  type Unit Either = Void
  unitorLeft (Right b) = b
  unitorLeft (Left a) = exFalso a
  counitorLeft = Right
  unitorRight (Left a) = a
  unitorRight (Right a) = exFalso a
  counitorRight = Left

----------------------------------------------------------------------------
---------------------------------- VOID ------------------------------------
----------------------------------------------------------------------------

newtype Void = Void Void

exFalso :: Void -> a
exFalso = undefined

exFalsoF :: Functor f => f Void -> f a
exFalsoF = fmap exFalso

deriving instance Generic Void

instance Eq Void where
  _ == _ = True

instance Ord Void where
  compare _ _ = EQ

instance Show Void where
  show = exFalso
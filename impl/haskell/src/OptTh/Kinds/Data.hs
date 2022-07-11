module OptTh.Kinds.Data where

import OptTh.Prelude
import Data.Functor.Identity (Identity)
import Data.Data (Proxy)
import Algebra.PartialOrd (PartialOrd (..))
import Data.Bifunctor
import GHC.Generics (Generic)

type OpticKind' = OpticKind Identity

data OpticKind m where
  Equality        :: OpticKind'
  Iso             :: OpticKind'
  Lens            :: OpticKind'
  Prism           :: OpticKind'
  Grate           :: OpticKind'
  Glass           :: OpticKind'
  Kaleidoscope    :: OpticKind'
  AffineTraversal :: OpticKind'
  Traversal1      :: OpticKind'
  Traversal       :: OpticKind'
  AffineFold      :: OpticKind'
  Fold1           :: OpticKind'
  Fold            :: OpticKind'
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
  unitorLeft (Left a) = absurd a
  counitorLeft = Right
  unitorRight (Left a) = a
  unitorRight (Right a) = absurd a
  counitorRight = Left
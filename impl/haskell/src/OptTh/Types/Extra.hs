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

----------------------------------------------------------------------------
----------------------------- APPLY ----------------------------------------
----------------------------------------------------------------------------

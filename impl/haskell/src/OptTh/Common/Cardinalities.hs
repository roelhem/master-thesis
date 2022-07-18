{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module OptTh.Common.Cardinalities where

import OptTh.Common.Functor
import GHC.TypeLits (Symbol)
import Data.Kind (Constraint)
import Data.Maybe (Maybe(..), listToMaybe, maybeToList, fromMaybe)
import Data.Semigroup (Semigroup)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Monoid (Monoid)
import Control.Category (Category(..))
import Algebra.Lattice.M2 (M2(..))
import Data.Profunctor (Profunctor(..))
import Data.Functor ((<$>), ($>))
import Data.Function (const, ($))
import Data.Foldable (toList)
import Data.List (head, tail)
import Control.Monad.State (State, MonadState (..), evalState)
import Data.Functor.Apply (WrappedApplicative(unwrapApplicative, WrapApplicative))

-- | Typeclass for the different kinds of cardinalities ("!", "+", "?", "*")
class Cardinality (k :: Symbol) where
  -- | The representation of this cardinality in the @M2@-lattice.
  cM2 :: M2

  -- | Converts a cardinality to its singular counterpart.
  type Singular    k :: Symbol
  -- | Converts a cardinality to its plural counterpart.
  type Plural      k :: Symbol
  -- | Converts a cardinality to its existential counterpart.
  type Existential k :: Symbol
  -- | Converts a cardinality to its optional counterpart.
  type Optional    k :: Symbol

  -- | The constraint for Applicative-like types.
  type AppLike   k (t :: * -> *) :: Constraint
  -- | The constraint for Foldable-like types.
  type FoldLike  k (t :: * -> *) :: Constraint
  -- | The constraint for Traversable-like types.
  type TravLike  k (t :: * -> *) :: Constraint
  -- | The constraint for Monoid-like types.
  type MonLike   k (m :: *)      :: Constraint
  -- | The type of the default container in Haskell that stores the items with this cardinality.
  type ConType   k :: * -> *
  -- | A data-type that can store the amount of items with this cardinality.
  data Container k :: * -> *

  -- | Converts a container to the standard Haskell representation.
  unC :: Container k a -> ConType k a
  
  -- | Converts the standard Haskell representation of values with this cardinality to the
  -- | @Container@ data type associated with this cardinality.
  toC :: ConType k a -> Container k a

  -- | Converts the standard Haskell representation to the plural counterpart.
  pluralize :: (Plural k ~ k', CardinalityMany k') => ConType k a -> ConType k' a
  default pluralize :: (Plural k ~ k', CardinalityMany k', k ~ k') => ConType k a -> ConType k' a
  pluralize = id

  -- | Converts the container to its plural counterpart.
  pluralizeC :: (Plural k ~ k', CardinalityMany k') => Container k a -> Container k' a
  pluralizeC = toC . pluralize @k . unC

  -- | Converts the standard Haskell representation to the singular counterpart.
  singularize :: (Singular k ~ k', CardinalitySingle k') => ConType k a -> ConType k' a
  default singularize :: (Singular k ~ k', CardinalitySingle k', k ~ k') => ConType k a -> ConType k' a
  singularize = id

  -- | Converts the container to its singular counterpart.
  singularizeC :: (Singular k ~ k', CardinalitySingle k') => Container k a -> Container k' a
  singularizeC = toC . singularize @k . unC

  -- | Converts the standard Haskell representation to the optional counterpart.
  optionalize :: (Optional k ~ k', CardinalityOptional k') => ConType k a -> ConType k' a
  default optionalize :: (Optional k ~ k', CardinalityOptional k', k ~ k') => ConType k a -> ConType k' a
  optionalize = id

  -- | Converts the container to its singular counterpart.
  optionalizeC :: (Optional k ~ k', CardinalityOptional k') => Container k a -> Container k' a
  optionalizeC = toC . optionalize @k . unC

  -- | Converts the standard Haskell representation to the existential counterpart, using the provided default
  -- | if it does not contain any value.
  existentialWith :: (Existential k ~ k', CardinalityExistential k') => a -> ConType k a -> ConType k' a
  default existentialWith :: (Existential k ~ k', CardinalityExistential k', k ~ k') => a -> ConType k a -> ConType k' a
  existentialWith _ = id

  -- | Converts the container to its existential counterpart, using the provided default
  -- | if it does not contain any value.
  existentialWithC :: (Existential k ~ k', CardinalityExistential k') => a -> Container k a -> Container k' a
  existentialWithC d = toC . existentialWith @k d . unC

  -- | Gets the tail of the container (all the values, except for the first one.)
  ctail :: Container k a -> [a]
  default ctail :: CardinalitySingle k => Container k a -> [a]
  ctail _ = []

  -- | General function to sequence a @TravLike k@ with an @AppLike k@. 
  cseq :: (TravLike k t, AppLike k f) => t (f a) -> f (t a)

  cpoint :: a -> ConType k a
  default cpoint :: Pointed (ConType k) => a -> ConType k a
  cpoint = point

  -- | Converts a traversal to a fold of the same cardinality.
  travToFold :: (MonLike  k m, AppLike  k (Const m)) => (forall f. (AppLike  k f) => (a -> f b) -> s -> f t) -> ((a -> m) -> s -> m)
  travToFold f = (getConst .) . f . (Const .)

  foldToCon :: (FoldLike k t) => t a -> ConType k a
  foldToC :: (FoldLike k t) => t a -> Container k a
  foldToC = toC . foldToCon @k

  -- | Generalizes an aggregrate function from the standard representation to the general one.
  generalizeAgg :: ((ConType k a -> b) -> ConType k s -> t) -> (forall c . TravLike k c => (c a -> b) -> c s -> t)

  -- | Type for pre-applied traversals.
  data PreApp k :: (* -> * -> *) -> * -> * -> * -> *

  -- | Make pre-applied traversals.
  mkPreApp  :: (forall f. (Functor f, AppLike k f) => p a (f b) -> f t) -> PreApp k p a b t
  runPreApp :: PreApp k p a b t -> (forall f. (Functor f, AppLike k f) => p a (f b) -> f t)
  runPreApplicative :: Functor (p a) => PreApp k p a b t -> (forall f. Applicative f => p a (f b) -> f t)

  -- | Constructor for free AppLike wrappers.
  data FreeApp k :: (* -> *) -> * -> *

  liftApC    :: f a -> FreeApp k f a
  runApC     :: AppLike k g => (forall x. f x -> g x) -> FreeApp k f a -> g a
  hoistApC   :: (forall a. f a -> g a) -> FreeApp k f b -> FreeApp k g b
  foldApC    :: (MonLike k m, AppLike k (Const m)) => (forall a. f a -> m) -> FreeApp k f a -> m
  foldApC f  = getConst . runApC (Const . f)
  retractApC :: AppLike k f => FreeApp k f a -> f a
  retractApC = runApC id

-- | Typeclass for cardinality types that have at most one value.
class (Cardinality k, k ~ Singular k) => CardinalitySingle (k :: Symbol) where
  runPrePointed :: Functor (p a) => PreApp k p a b t -> (forall f. (Functor f, Pointed f) => p a (f b) -> f t)
 
-- | Typeclass for cardinality types that allow for more than one value.
class (Cardinality k, k ~ Plural k) => CardinalityMany (k :: Symbol) where
  (<×>) :: AppLike k f => f (a -> b) -> f a -> f b

-- | Typeclass for cardinality types that always have at least one value.
class (Cardinality k, k ~ Existential k) => CardinalityExistential (k :: Symbol) where
  runPreApply :: Functor (p a) => PreApp k p a b t -> (forall f. Apply f => p a (f b) -> f t)
  chead :: Container k a -> a

-- | Typeclass for cadinality types that allows empty values.
class (Cardinality k, k ~ Optional k) => CardinalityOptional (k :: Symbol) where
  cempty :: Container k a
  cpure  :: AppLike k f => a -> f a

----------------------------------------------------------------------------
---------------------------------- ONE -------------------------------------
----------------------------------------------------------------------------

-- | A cardinality where there is always exactly one value.
instance Cardinality "!" where
  type Singular    "!" = "!"
  type Plural      "!" = "+"
  type Existential "!" = "!"
  type Optional    "!" = "?"

  type AppLike   "!" f = Functor f
  type FoldLike  "!" f = Copointed f
  type TravLike  "!" f = (Affine f, Traversable1 f)
  type MonLike   "!" m = ()
  type ConType   "!"   = Identity
  data Container "!" a = One a

  cM2 = M2o

  unC (One x)      = Identity x
  toC (Identity x) = One x

  pluralize   (Identity x) = x :| []
  optionalize (Identity x) = Just x

  cseq          = sequenceF
  foldToCon f   = Identity (copoint f)
  generalizeAgg o f s = o (\a -> f (runIdentity a <$ s)) (Identity $ copointFoldable1 s)

  data PreApp  "!" p a b t = BazaarF (forall f. Functor f => p a (f b) -> f t) --

  mkPreApp f            = BazaarF f
  runPreApp (BazaarF f) = f
  runPreApplicative (BazaarF f) = f

  data FreeApp "!" f a = ApC (forall g. Functor g => (forall x. f x -> g x) -> g a) --

  liftApC f = ApC (\k -> k f)
  runApC ϕ (ApC a) = a ϕ
  hoistApC ϕ (ApC g) = ApC (\k -> g (k . ϕ))

instance CardinalitySingle "!" where
  runPrePointed (BazaarF f) = f

instance CardinalityExistential "!" where
  runPreApply (BazaarF f) = f
  chead = runIdentity . unC

----------------------------------------------------------------------------
----------------------------- ONE OR MANY ----------------------------------
----------------------------------------------------------------------------

-- | A cardinality where there is at least one value (maybe more).
instance Cardinality "+" where
  type Singular    "+" = "!"
  type Plural      "+" = "+"
  type Existential "+" = "+"
  type Optional    "+" = "*"

  type AppLike   "+" f = Apply f
  type FoldLike  "+" f = Foldable1 f
  type TravLike  "+" f = Traversable1 f
  type MonLike   "+" m = Semigroup m
  type ConType   "+"   = NonEmpty
  data Container "+" a = OneOrMany a [a]

  cM2 = M2a

  unC (OneOrMany x xs) = x :| xs
  toC (x :| xs)        = OneOrMany x xs

  singularize (x :| _)  = Identity x
  optionalize (x :| xs) = x : xs

  ctail (OneOrMany _ xs) = xs

  cseq = sequence1

  foldToCon = toNonEmpty
  generalizeAgg o f s = o (f . evalState (mapM (const fillM) s) . toList ) (toNonEmpty s) where
    fillM :: State [a] a
    fillM = state (\as -> (head as, tail as))

  data PreApp     "+" p a b t = Bazaar1 (forall f. Apply f => p a (f b) -> f t) --
  mkPreApp f            = Bazaar1 f
  runPreApp (Bazaar1 f) = f
  runPreApplicative (Bazaar1 f) = dimap (fmap WrapApplicative) unwrapApplicative f 

  data FreeApp    "+" f a = ApCM (forall g. Apply g => (forall x. f x -> g x) -> g a) --
  liftApC f = ApCM (\k -> k f)
  runApC ϕ (ApCM a) = a ϕ
  hoistApC ϕ (ApCM g) = ApCM (\k -> g (k . ϕ))

instance CardinalityMany "+" where
  (<×>) = (<.>)

instance CardinalityExistential "+" where
  runPreApply (Bazaar1 f) = f
  chead (OneOrMany x _) = x

----------------------------------------------------------------------------
----------------------------- ZERO OR ONE ----------------------------------
----------------------------------------------------------------------------

-- | A cardinality that has at most one value.
instance Cardinality "?" where
  type Singular    "?" = "?"
  type Plural      "?" = "*"
  type Existential "?" = "!"
  type Optional    "?" = "?"

  type AppLike   "?" f = (Functor f, Pointed f)
  type FoldLike  "?" f = AffineFoldable f
  type TravLike  "?" f = (Affine f)
  type MonLike   "?" m = Default m
  type ConType   "?"   = Maybe
  data Container "?" a = ZeroOrOne (Maybe a)

  cM2 = M2b

  unC (ZeroOrOne m) = m
  toC               = ZeroOrOne

  pluralize         = maybeToList
  existentialWith x = Identity . fromMaybe x

  cseq = sequenceP

  foldToCon = toMaybe
  generalizeAgg o f s = o (f . evalState (mapM (const fillM) s) . toList ) (toMaybe s) where
    fillM :: State [a] a
    fillM = state (\as -> (head as, tail as))

  data PreApp  "?" p a b t = BazaarP (forall f. (Functor f, Pointed f) => p a (f b) -> f t) --
  mkPreApp f            = BazaarP f
  runPreApp (BazaarP f) = f
  runPreApplicative (BazaarP f) = dimap (fmap Θ1) unΘ1 f

  data FreeApp    "?" f a = ApCP (forall g. (Functor g, Pointed g) => (forall x. f x -> g x) -> g a) --
  liftApC f = ApCP (\k -> k f)
  runApC ϕ (ApCP a) = a ϕ
  hoistApC ϕ (ApCP g) = ApCP (\k -> g (k . ϕ))

instance CardinalitySingle "?" where
  runPrePointed (BazaarP f) = f

instance CardinalityOptional "?" where
  cempty = ZeroOrOne Nothing
  cpure  = point

----------------------------------------------------------------------------
----------------------------- ONE OR MANY ----------------------------------
----------------------------------------------------------------------------

-- | A cardinality that has any amount of values (maybe none).
instance Cardinality "*" where
  type Singular    "*" = "?"
  type Plural      "*" = "*"
  type Existential "*" = "+"
  type Optional    "*" = "*"

  type AppLike   "*" f = Applicative f
  type FoldLike  "*" f = Foldable f
  type TravLike  "*" f = Traversable f
  type MonLike   "*" m = Monoid m
  type ConType   "*"   = []
  data Container "*" a = ZeroOrMany [a]

  cM2 = M2i

  unC (ZeroOrMany as) = as
  toC                 = ZeroOrMany

  singularize              = listToMaybe
  existentialWith x []     = x :| []
  existentialWith _ (x:xs) = x :| xs

  ctail (ZeroOrMany [])     = []
  ctail (ZeroOrMany (_:xs)) = xs

  cseq = sequenceA
  
  foldToCon = toList
  generalizeAgg o f s = o ( f . evalState (mapM (const fillM) s) ) (toList s) where
    fillM :: State [a] a
    fillM = state (\as -> (head as, tail as))

  data PreApp  "*" p a b t = Bazaar (forall f. Applicative f => p a (f b) -> f t) --
  mkPreApp f                   = Bazaar f
  runPreApp (Bazaar f)         = f
  runPreApplicative (Bazaar f) = f

  data FreeApp "*" f a = ApCMP (forall g. Applicative g => (forall x. f x -> g x) -> g a) --
  liftApC f = ApCMP (\k -> k f)
  runApC ϕ (ApCMP a) = a ϕ
  hoistApC ϕ (ApCMP g) = ApCMP (\k -> g (k . ϕ))

instance CardinalityMany "*" where
  (<×>) = (<*>)

instance CardinalityOptional "*" where
  cempty = ZeroOrMany []
  cpure  = pure

----------------------------------------------------------------------------
-------------------------- GENERAL DEFINITIONS -----------------------------
----------------------------------------------------------------------------

instance Cardinality k => Functor (PreApp k p a b) where
  fmap f x = mkPreApp (fmap f . runPreApp x)

instance (Cardinality k, Profunctor p) => Profunctor (PreApp k p a) where
  dimap f g x = mkPreApp (\p -> g <$> runPreApp x (rmap (fmap f) p))

instance (CardinalityOptional k) => Pointed (PreApp k p a b) where
  point x = mkPreApp (const $ cpure @k x)

instance (CardinalityMany k) => Apply (PreApp k p a b) where
  f <.> x = mkPreApp (\p -> (<×>) @k (runPreApp f p) (runPreApp x p))

instance (CardinalityMany k, CardinalityOptional k) => Applicative (PreApp k p a b) where
  pure  = point
  (<*>) = (<.>)
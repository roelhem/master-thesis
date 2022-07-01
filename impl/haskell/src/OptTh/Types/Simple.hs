{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module OptTh.Types.Simple where

import qualified OptTh.Types.Kinds as K
import qualified Control.Lens as L
import Data.Functor.Identity (Identity (..))
import Data.Functor.Apply (Apply, MaybeApply (MaybeApply))
import Data.Bifunctor (first)
import Control.Comonad (Comonad (extract))
import Control.Applicative (Const(..))
import Data.Kind (Constraint)
import Control.Lens.Internal.Setter (Settable(..))
import Control.Monad ((>=>))

----------------------------------------------------------------------------
------------------------ SIMPLE OPTICS TYPE ALIASES ------------------------
----------------------------------------------------------------------------

type Equality        = Optic K.Equality        Identity
type Iso             = Optic K.Iso             Identity
type Setter          = Optic K.Setter          Identity
type Lens            = Optic K.Lens            Identity
type AlgLens         = Optic K.AlgLens
type AchromaticLens  = Optic K.AlgLens         Maybe
type Prism           = Optic K.Prism           Identity
type AlgPrism        = Optic K.AlgPrism
type Grate           = Optic K.Grate           Identity
type Glass           = Optic K.Glass           Identity
type Traversal       = Optic K.Traversal       Identity
type Traversal1      = Optic K.Traversal1      Identity
type AffineTraversal = Optic K.AffineTraversal Identity
type Kaleidoscope    = Optic K.Kaleidoscope    Identity
type Getter          = Optic K.Getter          Identity
type Fold            = Optic K.Fold            Identity
type Fold1           = Optic K.Fold1           Identity
type AffineFold      = Optic K.AffineFold      Identity
type Review          = Optic K.Review          Identity
type Unknown         = Optic K.Unknown         Identity

type Equality'          s a = Equality s s a a
type Iso'               s a = Iso s s a a
type Setter'            s a = Setter s s a a
type Lens'              s a = Lens s s a a
type AlgLens'         m s a = AlgLens m s s a a
type AchromaticLens'    s a = AchromaticLens s s a a
type Prism'             s a = Prism s s a a
type AlgPrism'        m s a = AlgPrism m s s a a
type Grate'             s a = Grate s s a a
type Glass'             s a = Glass s s a a
type Traversal'         s a = Traversal s s a a
type Traversal1'        s a = Traversal1 s s a a
type AffineTraversal'   s a = AffineTraversal s s a a
type Kaleidoscope'      s a = Kaleidoscope s s a a
type Getter'            s a = Getter s s a a
type Fold'              s a = Fold s s a a
type Fold1'             s a = Fold1 s s a a
type AffineFold'        s a = AffineFold s s a a
type Review'            s a = Review s s a a
type Unknown'           s a = Unknown s s a a

----------------------------------------------------------------------------
---------------------- SIMPLE OPTIC GADT DEFINITIONS -----------------------
----------------------------------------------------------------------------

data Optic (k :: K.OpticKind) m s t a b where
  Equality        :: (s ~ a, t ~ b) => Equality s t a b
  Iso             :: (s -> a)
                  -> (b -> t)
                  -> Iso s t a b
  Setter          :: ((a -> b) -> (s -> t))
                  -> Setter s t a b
  Lens            :: (s -> a)
                  -> (s -> b -> t)
                  -> Lens s t a b
  AlgLens         :: Monad m => (s -> a)
                  -> (m s -> b -> t)
                  -> AlgLens m s t a b
  Prism           :: (s -> Either t a)
                  -> (b -> t)
                  -> Prism s t a b
  AlgPrism        :: Comonad c => (s -> Either (c t) a)
                  -> (b -> t)
                  -> AlgPrism c s t a b
  Grate           :: (((s -> a) -> b) -> t)
                  -> Grate s t a b
  Glass           :: (s -> ((s -> a) -> b) -> t)
                  -> Glass s t a b
  AffineTraversal :: (s -> Either t (a, b -> t))
                  -> AffineTraversal s t a b
  Traversal       :: (forall f. (Apply f, Applicative f) => (a -> f b) -> s -> f t)
                  -> Traversal s t a b
  Traversal1      :: (forall f. Apply f => (a -> f b) -> s -> f t)
                  -> Traversal1 s t a b
  Kaleidoscope    :: (([a] -> b) -> [s] -> t)
                  -> Kaleidoscope s t a b
  Getter          :: (s -> a)
                  -> Getter s t a b
  Fold            :: (forall m. Monoid m => (a -> m) -> s -> m)
                  -> Fold s t a b
  Fold1           :: (forall g. Semigroup g => (a -> g) -> s -> g)
                  -> Fold1 s t a b
  AffineFold      :: (s -> Maybe a)
                  -> AffineFold s t a b
  Review          :: (b -> t)
                  -> Review s t a b
  Unknown         :: Unknown s t a b

----------------------------------------------------------------------------
--------------------------- HELPER FUNCTIONS -------------------------------
----------------------------------------------------------------------------

get :: (o ~> Getter) => o s t a b -> (s -> a)
get o = case og @_ @Getter o of Getter f -> f

put :: (o ~> Lens) => o s t a b -> (s -> b -> t)
put o = case og @_ @Lens o of Lens _ f -> f

over :: (o ~> Setter) => o s t a b -> ((a -> b) -> (s -> t))
over o = case og @_ @Setter o of Setter f -> f

set :: (o ~> Setter) => o s t a b -> (b -> s -> t)
set o b = over o (const b)

traversing :: (o ~> Traversal) => o s t a b -> (forall f. (Apply f, Applicative f) => (a -> f b) -> s -> f t)
traversing o = case og @_ @Traversal o of Traversal f -> f

travMaybe :: (o ~> Traversal) => o s t a b -> (forall f. Apply f => (a -> f b) -> s -> MaybeApply f t)
travMaybe o f = traversing o (MaybeApply . Left . f)

traversing1 :: (o ~> Traversal1) => o s t a b -> (forall f. Apply f => (a -> f b) -> s -> f t)
traversing1 o = case og @_ @Traversal1 o of Traversal1 f -> f

preview :: (o ~> AffineFold) => o s t a b -> (s -> Maybe a)
preview o = case og @_ @AffineFold o of AffineFold f -> f

folding :: (o ~> Fold) => o s t a b -> (forall m. Monoid m => (a -> m) -> s -> m)
folding o = case og @_ @Fold o of Fold f -> f

folding1 :: (o ~> Fold1) => o s t a b -> (forall m. Semigroup m => (a -> m) -> s -> m)
folding1 o = case og @_ @Fold1 o of Fold1 f -> f

collecting :: (o ~> Kaleidoscope) => o s t a b -> (([a] -> b) -> [s] -> t)
collecting o = case og @_ @Kaleidoscope o of Kaleidoscope f -> f

review :: (o ~> Review) => o s t a b -> (b -> t)
review o = case og @_ @Review o of Review f -> f

----------------------------------------------------------------------------
------------------------------- SUB OPTICS ---------------------------------
----------------------------------------------------------------------------

class (~>) o o' where
  og :: o s t a b -> o' s t a b

instance a ~> a where og = id

-- Equality ----------------------------------------------------------------

instance Equality ~> Iso where
  og Equality = Iso id id

-- Transitive inclusions
instance Monad m   => Equality ~> AlgLens m  where og = og @Iso . og
instance Comonad c => Equality ~> AlgPrism c where og = og @Iso . og
instance Equality ~> Lens                    where og = og @Iso . og
instance Equality ~> Prism                   where og = og @Iso . og
instance Equality ~> AffineTraversal         where og = og @Iso . og
instance Equality ~> Traversal1              where og = og @Iso . og
instance Equality ~> Glass                   where og = og @Iso . og
instance Equality ~> Traversal               where og = og @Iso . og
instance Equality ~> Setter                  where og = og @Iso . og
instance Equality ~> Getter                  where og = og @Iso . og
instance Equality ~> AffineFold              where og = og @Iso . og
instance Equality ~> Fold1                   where og = og @Iso . og
instance Equality ~> Fold                    where og = og @Iso . og
instance Equality ~> Review                  where og = og @Iso . og

-- Iso ---------------------------------------------------------------------

instance Monad m => Iso ~> AlgLens m where
  og (Iso get from) = AlgLens get (const from)

instance Comonad c => Iso ~> AlgPrism c where
  og (Iso get from) = AlgPrism (Right . get) from

instance Iso ~> Grate where
  og (Iso get from) = Grate $ \f -> from (f get)

instance Iso ~> Kaleidoscope where
  og (Iso get from) = Kaleidoscope (\f ss -> from $ (f.fmap get) ss)

-- Transitive inclusions
instance Iso ~> Lens  where og = og @(AlgLens  Identity) . og
instance Iso ~> Prism where og = og @(AlgPrism Identity) . og

-- Transitive inclusions
instance Iso ~> AffineTraversal where og = og @Lens . og
instance Iso ~> Traversal1      where og = og @Lens . og
instance Iso ~> Glass           where og = og @Lens . og
instance Iso ~> Traversal       where og = og @Lens . og
instance Iso ~> Setter          where og = og @Lens . og
instance Iso ~> Getter          where og = og @Lens . og
instance Iso ~> AffineFold      where og = og @Lens . og
instance Iso ~> Fold1           where og = og @Lens . og
instance Iso ~> Fold            where og = og @Lens . og
instance Iso ~> Review          where og = og @Prism . og

-- AlgLens -----------------------------------------------------------------

instance Monad m => AlgLens m ~> Lens where
  og (AlgLens get put) = Lens get $ \s -> put (return s)

-- Transitive inclusions
instance Monad c => AlgLens c ~> AffineTraversal where og = og @Lens . og
instance Monad c => AlgLens c ~> Traversal1      where og = og @Lens . og
instance Monad c => AlgLens c ~> Glass           where og = og @Lens . og
instance Monad c => AlgLens c ~> Traversal       where og = og @Lens . og
instance Monad c => AlgLens c ~> Setter          where og = og @Lens . og
instance Monad c => AlgLens c ~> Getter          where og = og @Lens . og
instance Monad c => AlgLens c ~> Fold1           where og = og @Lens . og
instance Monad c => AlgLens c ~> AffineFold      where og = og @Lens . og
instance Monad c => AlgLens c ~> Fold            where og = og @Lens . og

-- AchromaticLens ----------------------------------------------------------

instance AchromaticLens ~> Review where
  og (AlgLens get put) = Review (put Nothing)

-- ListLens ----------------------------------------------------------------

instance AlgLens [] ~> Kaleidoscope where
  og (AlgLens get put) = Kaleidoscope $ \f ss -> put ss ((f . fmap get) ss)

-- AlgPrism ----------------------------------------------------------------

instance Comonad c => AlgPrism c ~> Prism where
  og (AlgPrism preview review) = Prism (first extract <$> preview) review

-- Transitive inclusions
instance Comonad c => AlgPrism c ~> AffineTraversal where og = og @Prism . og
instance Comonad c => AlgPrism c ~> Traversal       where og = og @Prism . og
instance Comonad c => AlgPrism c ~> Setter          where og = og @Prism . og
instance Comonad c => AlgPrism c ~> AffineFold      where og = og @Prism . og
instance Comonad c => AlgPrism c ~> Fold            where og = og @Prism . og
instance Comonad c => AlgPrism c ~> Review          where og = og @Prism . og

-- Lens --------------------------------------------------------------------

instance Lens ~> AffineTraversal where
  og (Lens get put) = AffineTraversal (\s -> Right (get s, put s))

instance Lens ~> Traversal1 where
  og (Lens get put) = Traversal1 (\f s -> put s <$> f (get s))

instance Lens ~> Glass where
  og (Lens get put) = Glass $ \s f -> put s (f get)

instance Lens ~> Getter where
  og (Lens get _)   = Getter get

-- Transitive inclusions
instance Lens ~> Traversal  where og = og @AffineTraversal . og
instance Lens ~> Setter     where og = og @AffineTraversal . og
instance Lens ~> Fold1      where og = og @Getter . og
instance Lens ~> Fold       where og = og @Getter . og
instance Lens ~> AffineFold where og = og @Getter . og

-- Prism -------------------------------------------------------------------

instance Prism ~> AffineTraversal where
  og (Prism preview review) = AffineTraversal (fmap (,review) <$> preview)

instance Prism ~> Review where
  og (Prism _ review) = Review review

-- Transitive inclusions
instance Prism ~> Traversal  where og = og @AffineTraversal . og
instance Prism ~> Setter     where og = og @AffineTraversal . og
instance Prism ~> AffineFold where og = og @AffineTraversal . og
instance Prism ~> Fold       where og = og @AffineTraversal . og

-- Grate -------------------------------------------------------------------

instance Grate ~> Glass where
  og (Grate unzip) = Glass $ const unzip

instance Grate ~> Review where
  og (Grate unzip) = Review (unzip . const)

-- Transitive inclusions
instance Grate ~> Setter where og = og @Glass . og

-- Traversal1 --------------------------------------------------------------

instance Traversal1 ~> Traversal where
  og (Traversal1 trav) = Traversal trav

instance Traversal1 ~> Fold1 where
  og (Traversal1 trav) = Fold1 (\f -> getConst . trav (Const <$> f))

-- Transitive inclusions
instance Traversal1 ~> Setter where og = og @Traversal . og
instance Traversal1 ~> Fold   where og = og @Traversal . og

-- AffineTraversal ---------------------------------------------------------

instance AffineTraversal ~> Traversal where
  og (AffineTraversal unzip) = Traversal $ \f s -> case unzip s of
                                  Left t            -> pure t
                                  Right (a, review) -> review <$> f a

instance AffineTraversal ~> AffineFold where
  og (AffineTraversal unzip) = AffineFold (either (const Nothing) (Just . fst) <$> unzip)

-- Transitive inclusions
instance AffineTraversal ~> Setter where og = og @Traversal . og
instance AffineTraversal ~> Fold   where og = og @Traversal . og

-- Traversal ---------------------------------------------------------------

instance Traversal ~> Setter where
  og (Traversal trav) = Setter (\f s -> runIdentity $ trav (Identity <$> f) s)

instance Traversal ~> Fold where
  og (Traversal trav) = Fold (\f -> getConst . trav (Const <$> f))

-- Glass -------------------------------------------------------------------

instance Glass ~> Setter where
  og (Glass unzip) = Setter (\f s -> unzip s (\g -> f $ g s))

-- Kaleidoscope -------------------------------------------------------------

instance Kaleidoscope ~> Setter where
  og (Kaleidoscope collect) = Setter (\f s -> collect (f . head) [s])

instance Kaleidoscope ~> Review where
  og (Kaleidoscope collect) = Review (\b -> collect (const b) [])

-- Getter -------------------------------------------------------------------

instance Getter ~> Fold1 where
  og (Getter get) = Fold1 $ \f s -> f (get s)

instance Getter ~> AffineFold where
  og (Getter get) = AffineFold (Just . get)

-- Transitive inclusions
instance Getter ~> Fold where og = og @Fold1 . og

-- Fold1 --------------------------------------------------------------------

instance Fold1 ~> Fold where
  og (Fold1 fold) = Fold fold

-- AffineFold ---------------------------------------------------------------

instance AffineFold ~> Fold where
  og (AffineFold unzip) = Fold $ \ f -> maybe mempty f . unzip

-- Unknown -----------------------------------------------------------------

instance a ~> Unknown where og _ = Unknown

----------------------------------------------------------------------------
-------------------------------- MEETS -------------------------------------
----------------------------------------------------------------------------

type family (:\/:) (l :: * -> * -> * -> * -> *) (r :: * -> * -> * -> * -> *) :: * -> * -> * -> * -> * where
  o               :\/: o               = o
  Iso             :\/: Equality        = Iso
  Equality        :\/: r               = r
  Iso             :\/: r               = r
  Unknown         :\/: r               = Unknown
  Lens            :\/: (AlgLens m)     = Lens
  Lens            :\/: Prism           = AffineTraversal
  Lens            :\/: (AlgPrism m)    = AffineTraversal
  Lens            :\/: Grate           = Glass
  Lens            :\/: Glass           = Glass
  Lens            :\/: AffineTraversal = AffineTraversal
  Lens            :\/: Traversal1      = Traversal1
  Lens            :\/: Traversal       = Traversal
  Lens            :\/: AffineFold      = AffineFold
  Lens            :\/: Fold1           = Fold1
  Lens            :\/: Fold            = Fold
  Lens            :\/: Kaleidoscope    = Setter
  Lens            :\/: Setter          = Setter
  Lens            :\/: Getter          = Getter
  Lens            :\/: Review          = Unknown
  (AlgLens m)     :\/: r               = Lens :\/: r
  Prism           :\/: (AlgPrism m)    = Prism
  Prism           :\/: Grate           = Setter
  Prism           :\/: Glass           = Setter
  Prism           :\/: AffineTraversal = AffineTraversal
  Prism           :\/: Traversal1      = Traversal
  Prism           :\/: Traversal       = Traversal
  Prism           :\/: AffineFold      = Unknown
  Prism           :\/: Fold1           = Unknown
  Prism           :\/: Fold            = Unknown
  Prism           :\/: Kaleidoscope    = Setter
  Prism           :\/: Setter          = Setter
  Prism           :\/: Getter          = Unknown
  Prism           :\/: Review          = Review
  (AlgPrism c)    :\/: r               = Prism :\/: r
  Grate           :\/: Glass           = Glass
  Grate           :\/: AffineTraversal = Setter
  Grate           :\/: Traversal1      = Setter
  Grate           :\/: Traversal       = Setter
  Grate           :\/: AffineFold      = Unknown
  Grate           :\/: Fold1           = Unknown
  Grate           :\/: Fold            = Unknown
  Grate           :\/: Kaleidoscope    = Setter
  Grate           :\/: Setter          = Setter
  Grate           :\/: Getter          = Unknown
  Grate           :\/: Review          = Review
  Glass           :\/: AffineTraversal = Setter
  Glass           :\/: Traversal1      = Setter
  Glass           :\/: Traversal       = Setter
  Glass           :\/: AffineFold      = Unknown
  Glass           :\/: Fold1           = Unknown
  Glass           :\/: Fold            = Unknown
  Glass           :\/: Kaleidoscope    = Setter
  Glass           :\/: Setter          = Setter
  Glass           :\/: Getter          = Unknown
  Glass           :\/: Review          = Unknown
  AffineTraversal :\/: Traversal1      = Traversal
  AffineTraversal :\/: Traversal       = Traversal
  AffineTraversal :\/: AffineFold      = AffineFold
  AffineTraversal :\/: Fold1           = Fold
  AffineTraversal :\/: Fold            = Fold
  AffineTraversal :\/: Kaleidoscope    = Setter
  AffineTraversal :\/: Setter          = Setter
  AffineTraversal :\/: Getter          = AffineFold
  AffineTraversal :\/: Review          = Unknown
  Traversal1      :\/: Traversal       = Traversal
  Traversal1      :\/: AffineFold      = Fold
  Traversal1      :\/: Fold1           = Fold1
  Traversal1      :\/: Fold            = Fold
  Traversal1      :\/: Kaleidoscope    = Setter
  Traversal1      :\/: Setter          = Setter
  Traversal1      :\/: Getter          = Fold1
  Traversal1      :\/: Review          = Unknown
  Traversal       :\/: AffineFold      = Fold
  Traversal       :\/: Fold1           = Fold
  Traversal       :\/: Fold            = Fold
  Traversal       :\/: Kaleidoscope    = Setter
  Traversal       :\/: Setter          = Setter
  Traversal       :\/: Getter          = Fold
  Traversal       :\/: Review          = Unknown
  AffineFold      :\/: Fold1           = Fold
  AffineFold      :\/: Fold            = Fold
  AffineFold      :\/: Kaleidoscope    = Unknown
  AffineFold      :\/: Setter          = Unknown
  AffineFold      :\/: Getter          = AffineFold
  AffineFold      :\/: Review          = Unknown
  Fold1           :\/: Fold            = Fold
  Fold1           :\/: Kaleidoscope    = Unknown
  Fold1           :\/: Setter          = Unknown
  Fold1           :\/: Getter          = Fold1
  Fold1           :\/: Review          = Unknown
  Fold            :\/: Kaleidoscope    = Unknown
  Fold            :\/: Setter          = Unknown
  Fold            :\/: Getter          = Fold
  Fold            :\/: Review          = Unknown
  Kaleidoscope    :\/: Setter          = Setter
  Kaleidoscope    :\/: Getter          = Unknown
  Kaleidoscope    :\/: Review          = Review
  Setter          :\/: Getter          = Unknown
  Setter          :\/: Review          = Unknown
  Getter          :\/: Review          = Unknown
  l               :\/: r               = r :\/: l

----------------------------------------------------------------------------
----------------------------- COMPOSITIONS ---------------------------------
----------------------------------------------------------------------------

(>!>) :: Optic k m s t x y -> Optic k m x y a b -> Optic k m s t a b
Equality                 >!> Equality                   = Equality
Iso  get review          >!> Iso get' review'           = Iso (get'.get) (review . review')
Lens get put             >!> Lens get' put'             = Lens (get'.get)      $ \s b -> put s (put' (get s)     b)
AlgLens get put          >!> AlgLens get' put'          = AlgLens (get' . get) $ \s b -> put s (put' (get <$> s) b)
Prism matching review    >!> Prism matching' review'    = Prism (\s -> case matching s of
                                                                        Left  t -> Left t
                                                                        Right x -> case matching' x of
                                                                          Left  y -> Left (review y)
                                                                          Right a -> Right a
                                                                ) (review . review')
AlgPrism matching review >!> AlgPrism matching' review' = AlgPrism (\s -> case matching s of
                                                                           Left  t -> Left t
                                                                           Right x -> case matching' x of
                                                                             Left  y -> Left ( review <$> y)
                                                                             Right a -> Right a
                                                                   ) (review . review')
Grate unzip              >!> Grate unzip'               = Grate $ \f   -> unzip   (\g -> unzip'       (\g' -> f (g' . g)))
Glass unzip              >!> Glass unzip'               = Glass $ \s f -> unzip s (\g -> unzip' (g s) (\g' -> f (g' . g)))
AffineTraversal unzip    >!> AffineTraversal unzip'     = AffineTraversal $ \s -> case unzip s of
                                                                                  Left t -> Left t
                                                                                  Right (x, review) -> case unzip' x of
                                                                                    Left  y            -> Left (review y)
                                                                                    Right (a, review') -> Right (a, review . review')
Traversal1 trav          >!> Traversal1 trav'           = Traversal1   $ trav . trav'
Traversal trav           >!> Traversal trav'            = Traversal    $ trav . trav'
AffineFold preview       >!> AffineFold preview'        = AffineFold   $ preview >=> preview'
Fold1 folding            >!> Fold1 folding'             = Fold1        $ folding . folding'
Fold folding             >!> Fold folding'              = Fold         $ folding . folding'
Kaleidoscope collect     >!> Kaleidoscope collect'      = Kaleidoscope $ collect . collect'
Setter over              >!> Setter over'               = Setter       $ over . over'
Getter get               >!> Getter get'                = Getter       $ get' . get
Review review            >!> Review review'             = Review       $ review . review'
Unknown                  >!> Unknown                    = Unknown

(>?>) :: (l ~ Optic lo lm, r ~ Optic ro rm, l ~> o, r ~> o, o ~ Optic oo om) => l s t x y -> r x y a b -> o s t a b
l >?> r = og l >!> og r

(>.>) :: (l ~ Optic lo lm, r ~ Optic ro rm, l ~> o, r ~> o, o ~ (l :\/: r), o ~ Optic oo om) => l s t x y -> r x y a b -> o s t a b
(>.>) = (>?>)

----------------------------------------------------------------------------
------------------------------- INSTANCES ----------------------------------
----------------------------------------------------------------------------

----------------------------------------------------------------------------
---------------------- VAN LAARHOVEN REPRESENTATION ------------------------
----------------------------------------------------------------------------

class VLRepresentable o where
  type VLConstraint o (f :: * -> *) :: Constraint
  toVL   :: forall s t a b . o s t a b -> (forall f. VLConstraint o f => ((a -> f b) -> s -> f t))
  fromVL :: forall s t a b . (forall f . VLConstraint o f => ((a -> f b) -> s -> f t)) -> o s t a b

instance VLRepresentable Lens where
  type VLConstraint Lens f = (Functor f)
  toVL (Lens get put) f s = put s <$> f (get s)
  fromVL k = Lens (getConst . k Const) (\s b -> runIdentity $ k (const $ Identity b) s)

instance VLRepresentable Traversal1 where
  type VLConstraint Traversal1 f = (Apply f)
  toVL (Traversal1 trav) = trav
  fromVL = Traversal1

instance VLRepresentable Setter where
  type VLConstraint Setter f = (Settable f)
  toVL (Setter over) f = pure . over (untainted . f)
  fromVL d = Setter (\f -> runIdentity . d (Identity . f))
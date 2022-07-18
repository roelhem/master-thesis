module OptTh.Simple.Types where

import qualified OptTh.Kinds.Optics as K
import qualified Control.Lens as L
import OptTh.Prelude
import Data.Bifunctor (first)
import Control.Comonad (Comonad (extract))
import Control.Applicative (Const(..))
import Data.Kind (Constraint)
import Control.Monad ((>=>))
import OptTh.Common.Categories (Composable (..), Composable' (..), type (~>) (..))
import OptTh.Common.Profunctor
import OptTh.Common.Staby
import OptTh.Common.Cardinalities
import OptTh.Common.Functor (Affine(..), Contravariant(..), AffineFoldable (..), Pointed (..))
import Data.Profunctor.Traversing (leftTraversing, rightTraversing, firstTraversing, secondTraversing)
import Data.Default (Default)
import Data.Foldable (Foldable(toList))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (listToMaybe, fromJust)
import Data.Semigroup (WrappedMonoid(..))
import Data.Functor.Apply (WrappedApplicative(..))

----------------------------------------------------------------------------
------------------------ SIMPLE OPTICS TYPE ALIASES ------------------------
----------------------------------------------------------------------------

type Equality           = Optic K.Equality
type Iso                = Optic K.Iso
type Lens               = Optic K.Lens
type AlgLens m          = Optic (K.AlgLens m)
type AchromaticLens     = Optic K.AchromaticLens
type ClassifyingLens    = Optic K.ClassifyingLens
type Prism              = Optic K.Prism
type AlgPrism m         = Optic (K.AlgPrism m)
type Grate              = Optic K.Grate
type Glass              = Optic K.Glass
type Traversal          = Optic K.Traversal
type Traversal1         = Optic K.Traversal1
type AffineTraversal    = Optic K.AffineTraversal
type Kaleidoscope       = Optic K.Kaleidoscope
type Kaleidoscope1      = Optic K.Kaleidoscope1
type AffineKaleidoscope = Optic K.AffineKaleidoscope
type Setter             = Optic K.Setter
type Getter             = Optic K.Getter
type Fold               = Optic K.Fold
type Fold1              = Optic K.Fold1
type AffineFold         = Optic K.AffineFold
type Review             = Optic K.Review
type Unknown            = Optic K.Unknown

type Equality'          s a = Equality         s s a a
type Iso'               s a = Iso              s s a a
type Setter'            s a = Setter           s s a a
type Lens'              s a = Lens             s s a a
type AlgLens'         m s a = AlgLens        m s s a a
type AchromaticLens'    s a = AchromaticLens   s s a a
type Prism'             s a = Prism            s s a a
type AlgPrism'        m s a = AlgPrism       m s s a a
type Grate'             s a = Grate            s s a a
type Glass'             s a = Glass            s s a a
type Traversal'         s a = Traversal        s s a a
type Traversal1'        s a = Traversal1       s s a a
type AffineTraversal'   s a = AffineTraversal  s s a a
type Kaleidoscope'      s a = Kaleidoscope     s s a a
type Getter'            s a = Getter           s s a a
type Fold'              s a = Fold             s s a a
type Fold1'             s a = Fold1            s s a a
type AffineFold'        s a = AffineFold       s s a a
type Review'            s a = Review           s s a a
type Unknown'           s a = Unknown          s s a a

----------------------------------------------------------------------------
---------------------- SIMPLE OPTIC GADT DEFINITIONS -----------------------
----------------------------------------------------------------------------

data Optic (k :: K.OpticKind) s t a b where
  Equality           :: (s ~ a, t ~ b) => Equality s t a b
  Iso                :: (s -> a)
                     -> (b -> t)
                     -> Iso s t a b
  Lens               :: (s -> a)
                     -> (s -> b -> t)
                     -> Lens s t a b
  AlgLens            :: Monad m => (s -> a)
                     -> (m s -> b -> t)
                     -> AlgLens m s t a b
  Prism              :: (s -> Either t a)
                     -> (b -> t)
                     -> Prism s t a b
  AlgPrism           :: Comonad c => (s -> Either (c t) a)
                     -> (b -> t)
                     -> AlgPrism c s t a b
  Grate              :: (((s -> a) -> b) -> t)
                     -> Grate s t a b
  Glass              :: (s -> ((s -> a) -> b) -> t)
                     -> Glass s t a b
  AffineTraversal    :: (s -> Either t (a, b -> t))
                     -> AffineTraversal s t a b
  Traversal          :: (forall f. Applicative f => (a -> f b) -> s -> f t)
                     -> Traversal s t a b
  Traversal1         :: (forall f. Apply f => (a -> f b) -> s -> f t)
                     -> Traversal1 s t a b
  Kaleidoscope       :: (([a] -> b) -> [s] -> t)
                     -> Kaleidoscope s t a b
  Kaleidoscope1      :: ((NonEmpty a -> b) -> NonEmpty s -> t)
                     -> Kaleidoscope1 s t a b
  AffineKaleidoscope :: ((Maybe a -> b) -> Maybe s -> t)
                     -> AffineKaleidoscope s t a b
  Setter             :: ((a -> b) -> (s -> t))
                     -> Setter s t a b
  Getter             :: (s -> a)
                     -> Getter s t a b
  Fold               :: (forall m. Monoid m => (a -> m) -> s -> m)
                     -> Fold s t a b
  Fold1              :: (forall g. Semigroup g => (a -> g) -> s -> g)
                     -> Fold1 s t a b
  AffineFold         :: (s -> Maybe a)
                     -> AffineFold s t a b
  Review             :: (b -> t)
                     -> Review s t a b
  Unknown            :: Unknown s t a b

----------------------------------------------------------------------------
------------------------- PROFUNCTOR INSTANCES -----------------------------
----------------------------------------------------------------------------

-- Iso

instance Staby Iso where
  smap f (Iso view review) = Iso (view . f) review
  tmap f (Iso view review) = Iso view (f . review)
  amap f (Iso view review) = Iso (f . view) review
  bmap f (Iso view review) = Iso view (review . f)

-- Setter

instance Staby Setter where
  smap f (Setter over) = Setter ((. f) . over)
  tmap f (Setter over) = Setter (fmap f . over)
  amap f (Setter over) = Setter (over . (.f))
  bmap f (Setter over) = Setter (over . (f .))

instance Closed (ABST Setter a b) where
  closed (ABST (Setter over)) = ABST $ Setter (fmap . over)

instance Choice (ABST Setter a b) where
  left'  (ABST (Setter over)) = ABST $ Setter (first  . over)
  right' (ABST (Setter over)) = ABST $ Setter (second . over)

instance Strong (ABST Setter a b) where
  first'  (ABST (Setter over)) = ABST $ Setter (first  . over)
  second' (ABST (Setter over)) = ABST $ Setter (second . over)

instance Traversing1      (ABST Setter a b) where wander1   f (ABST (Setter over)) = ABST $ Setter (fmap runIdentity . f . fmap Identity . over)
instance Traversing       (ABST Setter a b) where wander    f (ABST (Setter over)) = ABST $ Setter (fmap runIdentity . f . fmap Identity . over)

instance Mapping (ABST Setter a b)
-- Lens

instance Staby Lens where
  smap f (Lens view put) = Lens (view . f) (put . f)
  tmap f (Lens view put) = Lens view ((f .) . put)
  amap f (Lens view put) = Lens (f . view) put
  bmap f (Lens view put) = Lens view ((. f) <$> put)

instance Strong (ABST Lens a b) where
  first'  (ABST (Lens view put)) = ABST $ Lens (view . fst) (\(s,x) b -> (put s b, x))
  second' (ABST (Lens view put)) = ABST $ Lens (view . snd) (\(x,s) b -> (x, put s b))

-- AlgLens

instance Staby (AlgLens m) where
  smap f (AlgLens view put) = AlgLens (view . f) (put . fmap f)
  tmap f (AlgLens view put) = AlgLens view ((f .) . put)
  amap f (AlgLens view put) = AlgLens (f . view) put
  bmap f (AlgLens view put) = AlgLens view ((. f) <$> put)

-- Prism

instance Staby Prism where
  smap f (Prism matching review) = Prism (matching . f) review
  tmap f (Prism matching review) = Prism (first f . matching) (f . review)
  amap f (Prism matching review) = Prism (second f . matching) review
  bmap f (Prism matching review) = Prism matching (review . f)

instance Choice (ABST Prism a b) where
  left' (ABST (Prism matching review)) = ABST $ Prism (\case
      Left  s -> case matching s of
          Left  t -> Left $ Left t
          Right f -> Right f
      Right x -> Left $ Right x
    ) (Left . review)
  right' (ABST (Prism matching review)) = ABST $ Prism (\case
      Right s -> case matching s of
          Left  t -> Left $ Right t
          Right f -> Right f
      Left  x -> Left $ Left x
    ) (Right . review)

-- AlgPrism

instance Staby (AlgPrism m) where
  smap f (AlgPrism matching review) = AlgPrism (matching . f) review
  tmap f (AlgPrism matching review) = AlgPrism ((first . fmap) f . matching) (f . review)
  amap f (AlgPrism matching review) = AlgPrism (second f . matching) review
  bmap f (AlgPrism matching review) = AlgPrism matching (review . f)

-- Grate

instance Staby Grate where
  smap f (Grate rezip) = Grate (rezip . (. (.f)))
  tmap f (Grate rezip) = Grate (f . rezip)
  amap f (Grate rezip) = Grate (rezip . (. (f .)))
  bmap f (Grate rezip) = Grate (rezip . (f .))

instance Closed (ABST Grate a b) where
  closed (ABST (Grate rezip)) = ABST . Grate $ \f x -> rezip (\g -> f (\h -> (g . h) x))

-- Glass

instance Staby Glass where
  smap f (Glass rezip) = Glass (\s g -> rezip (f s) (g . (. f)))
  tmap f (Glass rezip) = Glass (fmap f . rezip)
  amap f (Glass rezip) = Glass ((. (. (f .))) . rezip)
  bmap f (Glass rezip) = Glass ((. (f .)) <$> rezip)

instance Strong (ABST Glass a b) where
  first'  (ABST (Glass rezip)) = ABST . Glass $ \(s,x) f -> (rezip s (f . (. fst)), x)
  second' (ABST (Glass rezip)) = ABST . Glass $ \(x,s) f -> (x, rezip s (f . (. snd)))

instance Closed (ABST Glass a b) where
  closed (ABST (Glass rezip)) = ABST . Glass $ \s f x -> rezip (s x) (\g -> f (\h -> (g . h) x))

-- AffineTraversal

instance Staby AffineTraversal where
  smap f (AffineTraversal affine) = AffineTraversal (affine . f)
  tmap f (AffineTraversal affine) = AffineTraversal ( bimap f ((fmap . fmap) f) . affine)
  amap f (AffineTraversal affine) = AffineTraversal ((second . first) f . affine)
  bmap f (AffineTraversal affine) = AffineTraversal ((second . second) (. f) . affine)

instance Strong (ABST AffineTraversal a b) where
  first'  (ABST (AffineTraversal affine)) = ABST . AffineTraversal $ \(s,c) -> bimap (,c) (second (\ f x -> (f x, c))) (affine s)
  second' (ABST (AffineTraversal affine)) = ABST . AffineTraversal $ \(c,s) -> bimap (c,) (second (\ f x -> (c, f x))) (affine s)

instance Choice (ABST AffineTraversal a b) where
  left'   (ABST (AffineTraversal affine)) = ABST . AffineTraversal $ \case
                                              Left  s -> bimap Left ((fmap .fmap) Left) (affine s)
                                              Right c -> (Left . Right) c
  right'  (ABST (AffineTraversal affine)) = ABST . AffineTraversal $ \case
                                              Right s -> bimap Right ((fmap .fmap) Right) (affine s)
                                              Left  c -> (Left . Left) c

-- Traversal

instance Staby Traversal where
  smap f (Traversal trav) = Traversal ((.f) . trav)
  tmap f (Traversal trav) = Traversal ((fmap . fmap) f . trav )
  amap f (Traversal trav) = Traversal (trav . (. f))
  bmap f (Traversal trav) = Traversal (trav . (fmap f .))

newtype Wα a b = Wα { unWα :: a b } deriving (Functor)

instance Applicative a => Pointed (Wα a) where
  point = pure

instance Applicative a => Applicative (Wα a) where
  pure x = Wα (pure x)
  Wα lhs <*> Wα rhs = Wα (lhs <*> rhs)

instance Traversing1      (ABST Traversal a b) where wander1   f (ABST (Traversal trav)) = ABST $ Traversal (\g -> unwrapApplicative . f (trav (WrapApplicative . g)))
instance Traversing       (ABST Traversal a b) where wander    f (ABST (Traversal trav)) = ABST $ Traversal (f . trav)

instance Strong (ABST Traversal a b) where first'  = firstTraversing ; second' = secondTraversing
instance Choice (ABST Traversal a b) where left'   = leftTraversing  ; right'  = rightTraversing

-- Traversal1

instance Staby Traversal1 where
  smap f (Traversal1 trav) = Traversal1 ((.f) . trav)
  tmap f (Traversal1 trav) = Traversal1 ((fmap . fmap) f . trav )
  amap f (Traversal1 trav) = Traversal1 (trav . (. f))
  bmap f (Traversal1 trav) = Traversal1 (trav . (fmap f .))

instance Strong (ABST Traversal1 a b) where
  first'  (ABST (Traversal1 trav)) = ABST $ Traversal1 (\f (s, c) -> (,c) <$> trav f s)
  second' (ABST (Traversal1 trav)) = ABST $ Traversal1 (\f (c, s) -> (c,) <$> trav f s)

instance Traversing1 (ABST Traversal1 a b) where
  wander1 f (ABST (Traversal1 trav)) = ABST $ Traversal1 (f . trav)

-- Kaleidoscope

instance Staby Kaleidoscope where
  smap f (Kaleidoscope agg) = Kaleidoscope ((.fmap f) . agg)
  tmap f (Kaleidoscope agg) = Kaleidoscope (fmap f . agg)
  amap f (Kaleidoscope agg) = Kaleidoscope (agg . (. fmap f))
  bmap f (Kaleidoscope agg) = Kaleidoscope (agg . (f .))

instance Closed (ABST Kaleidoscope a b) where closed = aggregate'

instance Aggregating (ABST Kaleidoscope a b) where
  aggregate' (ABST (Kaleidoscope agg)) = ABST . Kaleidoscope $ \f x -> agg f <$> sequenceA x

-- Kaleidoscope1

instance Staby Kaleidoscope1 where
  smap f (Kaleidoscope1 agg) = Kaleidoscope1 ((.fmap f) . agg)
  tmap f (Kaleidoscope1 agg) = Kaleidoscope1 (fmap f . agg)
  amap f (Kaleidoscope1 agg) = Kaleidoscope1 (agg . (. fmap f))
  bmap f (Kaleidoscope1 agg) = Kaleidoscope1 (agg . (f .))

instance Closed       (ABST Kaleidoscope1 a b) where closed  = aggregate'
instance Strong       (ABST Kaleidoscope1 a b) where second' = aggregating1Second'
instance Aggregating  (ABST Kaleidoscope1 a b) where
  aggregate'  (ABST (Kaleidoscope1 agg)) = ABST . Kaleidoscope1 $ \f x -> agg f <$> sequenceA x
instance Aggregating1 (ABST Kaleidoscope1 a b) where
  aggregate1' (ABST (Kaleidoscope1 agg)) = ABST . Kaleidoscope1 $ \f x -> agg f <$> sequence1 x

-- AffineKaleidoscope

instance Staby AffineKaleidoscope where
  smap f (AffineKaleidoscope agg) = AffineKaleidoscope ((.fmap f) . agg)
  tmap f (AffineKaleidoscope agg) = AffineKaleidoscope (fmap f . agg)
  amap f (AffineKaleidoscope agg) = AffineKaleidoscope (agg . (. fmap f))
  bmap f (AffineKaleidoscope agg) = AffineKaleidoscope (agg . (f .))

instance Choice            (ABST AffineKaleidoscope a b) where right' = aggregateAff'
instance Closed            (ABST AffineKaleidoscope a b) where closed = aggregate'
instance Aggregating       (ABST AffineKaleidoscope a b) where
  aggregate'    (ABST (AffineKaleidoscope agg)) = ABST . AffineKaleidoscope $ \f x -> agg f <$> sequenceA x
instance AffineAggregating (ABST AffineKaleidoscope a b) where
  aggregateAff' (ABST (AffineKaleidoscope agg)) = ABST . AffineKaleidoscope $ \f x -> agg f <$> sequenceP x

-- Getter

instance Staby Getter where
  smap f (Getter get) = Getter (get . f)
  tmap _ (Getter get) = Getter get
  amap f (Getter get) = Getter (f . get)
  bmap _ (Getter get) = Getter get

instance Bicontravariant (ABST Getter a b) where
  contrabimap f _ (ABST (Getter get)) = ABST . Getter $ get . f

-- Fold

instance Staby Fold where
  smap f (Fold folding) = Fold ((.f) . folding)
  tmap _ (Fold folding) = Fold folding
  amap f (Fold folding) = Fold (folding . (. f))
  bmap _ (Fold folding) = Fold folding

instance Bicontravariant (ABST Fold a b) where
  contrabimap f _ (ABST (Fold folding)) = ABST $ Fold $ \f' -> folding f' . f

instance Choice   (ABST Fold a b) where
  left' (ABST (Fold folding)) = ABST $ Fold $ \f -> \case
    Left  a -> folding f a
    Right c -> mempty

instance Traversing1      (ABST Fold a b) where wander1   f (ABST (Fold folding)) = ABST $ Fold $ travToFold @"+" f . folding
instance Traversing       (ABST Fold a b) where wander    f (ABST (Fold folding)) = ABST $ Fold $ travToFold @"*" f . folding

-- Fold1

instance Staby Fold1 where
  smap f (Fold1 folding) = Fold1 ((.f) . folding)
  tmap _ (Fold1 folding) = Fold1 folding
  amap f (Fold1 folding) = Fold1 (folding . (. f))
  bmap _ (Fold1 folding) = Fold1 folding

instance Bicontravariant (ABST Fold1 a b) where
  contrabimap f _ (ABST (Fold1 folding)) = ABST $ Fold1 $ \f' -> folding f' . f

instance Traversing1 (ABST Fold1 a b) where
  wander1 f (ABST (Fold1 folding)) = ABST $ Fold1 $ travToFold @"+" f . folding

-- (getConst .) . f . (Const .)

-- AffineFold

instance Staby AffineFold where
  smap f (AffineFold preview) = AffineFold (preview . f)
  tmap _ (AffineFold preview) = AffineFold preview
  amap f (AffineFold preview) = AffineFold (fmap f . preview)
  bmap _ (AffineFold preview) = AffineFold preview

instance Choice   (ABST AffineFold a b) where
  left' (ABST (AffineFold preview)) = ABST $ AffineFold $ \case
                                              Left  a -> preview a
                                              Right c -> Nothing
  right' (ABST (AffineFold preview)) = ABST $ AffineFold $ \case
                                               Right a -> preview a
                                               Left  c -> Nothing

instance Bicontravariant (ABST AffineFold a b) where
  contrabimap f _ (ABST (AffineFold preview)) = ABST $ AffineFold $ preview . f

-- Review

instance Staby Review where
  smap _ (Review review) = Review review
  tmap f (Review review) = Review (f . review)
  amap _ (Review review) = Review review
  bmap f (Review review) = Review (review . f)

instance Bifunctor (ABST Review a b) where
  bimap _ g (ABST (Review review)) = ABST $ Review $ g . review

instance Aggregating (ABST Review a b) where
  aggregate' (ABST (Review review)) = ABST $ Review $ pure . review

-- Unknown

instance Staby Unknown where
  stabmap _ _ _ _ _ = Unknown

instance Bicontravariant  (ABST Unknown a b) where contrabimap _ _ _ = ABST Unknown
instance Bifunctor        (ABST Unknown a b) where bimap _ _ _       = ABST Unknown
instance Traversing       (ABST Unknown a b) where wander        _ _ = ABST Unknown
instance Traversing1      (ABST Unknown a b) where wander1       _ _ = ABST Unknown
instance Closed           (ABST Unknown a b) where closed          _ = ABST Unknown
instance Mapping          (ABST Unknown a b) where roam          _ _ = ABST Unknown
instance Aggregating      (ABST Unknown a b) where aggregate'      _ = ABST Unknown

----------------------------------------------------------------------------
------------------------------- SUB OPTICS ---------------------------------
----------------------------------------------------------------------------

-- Equality ----------------------------------------------------------------

instance Equality ~> Iso where
  oTo Equality = Iso id id

-- Transitive inclusions
instance Monad m   => Equality ~> AlgLens m  where oTo = oTo @Iso . oTo
instance Comonad c => Equality ~> AlgPrism c where oTo = oTo @Iso . oTo
instance Equality ~> Lens                    where oTo = oTo @Iso . oTo
instance Equality ~> Prism                   where oTo = oTo @Iso . oTo
instance Equality ~> AffineTraversal         where oTo = oTo @Iso . oTo
instance Equality ~> Traversal1              where oTo = oTo @Iso . oTo
instance Equality ~> Glass                   where oTo = oTo @Iso . oTo
instance Equality ~> Traversal               where oTo = oTo @Iso . oTo
instance Equality ~> Setter                  where oTo = oTo @Iso . oTo
instance Equality ~> Getter                  where oTo = oTo @Iso . oTo
instance Equality ~> AffineFold              where oTo = oTo @Iso . oTo
instance Equality ~> Fold1                   where oTo = oTo @Iso . oTo
instance Equality ~> Fold                    where oTo = oTo @Iso . oTo
instance Equality ~> Review                  where oTo = oTo @Iso . oTo
instance Equality ~> Kaleidoscope            where oTo = oTo @Iso . oTo
instance Equality ~> Kaleidoscope1           where oTo = oTo @Iso . oTo
instance Equality ~> AffineKaleidoscope      where oTo = oTo @Iso . oTo

-- Iso ---------------------------------------------------------------------

instance Monad m => Iso ~> AlgLens m where
  oTo (Iso get from) = AlgLens get (const from)

instance Comonad c => Iso ~> AlgPrism c where
  oTo (Iso get from) = AlgPrism (Right . get) from

instance Iso ~> Grate where
  oTo (Iso get from) = Grate $ \f -> from (f get)

-- Transitive inclusions
instance Iso ~> Lens               where oTo = oTo @(AlgLens  Identity) . oTo
instance Iso ~> Prism              where oTo = oTo @(AlgPrism Identity) . oTo
instance Iso ~> AffineTraversal    where oTo = oTo @Lens . oTo
instance Iso ~> Traversal1         where oTo = oTo @Lens . oTo
instance Iso ~> Glass              where oTo = oTo @Lens . oTo
instance Iso ~> Traversal          where oTo = oTo @Lens . oTo
instance Iso ~> Setter             where oTo = oTo @Lens . oTo
instance Iso ~> Getter             where oTo = oTo @Lens . oTo
instance Iso ~> AffineFold         where oTo = oTo @Lens . oTo
instance Iso ~> Fold1              where oTo = oTo @Lens . oTo
instance Iso ~> Fold               where oTo = oTo @Lens . oTo
instance Iso ~> Kaleidoscope1      where oTo = oTo @Lens . oTo
instance Iso ~> Review             where oTo = oTo @Prism . oTo
instance Iso ~> Kaleidoscope       where oTo = oTo @Grate . oTo
instance Iso ~> AffineKaleidoscope where oTo = oTo @Grate . oTo

-- AlgLens -----------------------------------------------------------------

instance Monad m => AlgLens m ~> Lens where
  oTo (AlgLens get put) = Lens get $ \s -> put (return s)

-- Transitive inclusions
instance Monad c => AlgLens c ~> AffineTraversal where oTo = oTo @Lens . oTo
instance Monad c => AlgLens c ~> Traversal1      where oTo = oTo @Lens . oTo
instance Monad c => AlgLens c ~> Glass           where oTo = oTo @Lens . oTo
instance Monad c => AlgLens c ~> Traversal       where oTo = oTo @Lens . oTo
instance Monad c => AlgLens c ~> Setter          where oTo = oTo @Lens . oTo
instance Monad c => AlgLens c ~> Getter          where oTo = oTo @Lens . oTo
instance Monad c => AlgLens c ~> Fold1           where oTo = oTo @Lens . oTo
instance Monad c => AlgLens c ~> AffineFold      where oTo = oTo @Lens . oTo
instance Monad c => AlgLens c ~> Fold            where oTo = oTo @Lens . oTo
instance Monad c => AlgLens c ~> Kaleidoscope1   where oTo = oTo @Lens . oTo

-- AchromaticLens ----------------------------------------------------------

instance AchromaticLens ~> AffineKaleidoscope where
  oTo (AlgLens get put) = AffineKaleidoscope $ \f ss -> put ss ((f . fmap get) ss)

instance AchromaticLens ~> Review where oTo = oTo @AffineKaleidoscope . oTo

-- ClassifyingLens ---------------------------------------------------------

instance AlgLens [] ~> Kaleidoscope where
  oTo (AlgLens get put) = Kaleidoscope $ \f ss -> put ss ((f . fmap get) ss)

instance AlgLens [] ~> AffineKaleidoscope where oTo = oTo @Kaleidoscope . oTo
instance AlgLens [] ~> Review             where oTo = oTo @Kaleidoscope . oTo

-- AlgPrism ----------------------------------------------------------------

instance Comonad c => AlgPrism c ~> Prism where
  oTo (AlgPrism preview review) = Prism (first extract <$> preview) review

-- Transitive inclusions
instance Comonad c => AlgPrism c ~> AffineTraversal where oTo = oTo @Prism . oTo
instance Comonad c => AlgPrism c ~> Traversal       where oTo = oTo @Prism . oTo
instance Comonad c => AlgPrism c ~> Setter          where oTo = oTo @Prism . oTo
instance Comonad c => AlgPrism c ~> AffineFold      where oTo = oTo @Prism . oTo
instance Comonad c => AlgPrism c ~> Fold            where oTo = oTo @Prism . oTo
instance Comonad c => AlgPrism c ~> Review          where oTo = oTo @Prism . oTo

-- Lens --------------------------------------------------------------------

instance Lens ~> AffineTraversal where
  oTo (Lens get put) = AffineTraversal (\s -> Right (get s, put s))

instance Lens ~> Traversal1 where
  oTo (Lens get put) = Traversal1 (\f s -> put s <$> f (get s))

instance Lens ~> Glass where
  oTo (Lens get put) = Glass $ \s f -> put s (f get)

instance Lens ~> Getter where
  oTo (Lens get _)   = Getter get

-- Transitive inclusions
instance Lens ~> Traversal     where oTo = oTo @AffineTraversal . oTo
instance Lens ~> Setter        where oTo = oTo @AffineTraversal . oTo
instance Lens ~> Fold1         where oTo = oTo @Getter . oTo
instance Lens ~> Fold          where oTo = oTo @Getter . oTo
instance Lens ~> AffineFold    where oTo = oTo @Getter . oTo
instance Lens ~> Kaleidoscope1 where oTo = oTo @Glass . oTo

-- Prism -------------------------------------------------------------------

instance Prism ~> AffineTraversal where
  oTo (Prism matching review) = AffineTraversal (fmap (,review) <$> matching)

instance Prism ~> AffineKaleidoscope where
  oTo (Prism matching review) = AffineKaleidoscope $ \f -> \case
    Nothing -> review $ f Nothing
    Just s -> case matching s of
      Left t -> t
      Right a -> review . f $ Just a

-- Transitive inclusions
instance Prism ~> Traversal  where oTo = oTo @AffineTraversal . oTo
instance Prism ~> Setter     where oTo = oTo @AffineTraversal . oTo
instance Prism ~> AffineFold where oTo = oTo @AffineTraversal . oTo
instance Prism ~> Fold       where oTo = oTo @AffineTraversal . oTo
instance Prism ~> Review     where oTo = oTo @AffineKaleidoscope . oTo
-- Grate -------------------------------------------------------------------

instance Grate ~> Glass where
  oTo (Grate unzip) = Glass $ const unzip

instance Grate ~> Kaleidoscope where
  oTo (Grate unzip) = Kaleidoscope (\f s -> unzip $ \g -> f (g <$> s))

-- Transitive inclusions
instance Grate ~> Review             where oTo = oTo @Kaleidoscope . oTo
instance Grate ~> AffineKaleidoscope where oTo = oTo @Kaleidoscope . oTo
instance Grate ~> Setter             where oTo = oTo @Glass . oTo
instance Grate ~> Kaleidoscope1      where oTo = oTo @Glass . oTo

-- Traversal1 --------------------------------------------------------------

instance Traversal1 ~> Traversal where
  oTo (Traversal1 trav) = Traversal (dimap (WrapApplicative .) (unwrapApplicative .) trav)

instance Traversal1 ~> Fold1 where
  oTo (Traversal1 trav) = Fold1 (\f -> getConst . trav (Const <$> f))

-- Transitive inclusions
instance Traversal1 ~> Setter where oTo = oTo @Traversal . oTo
instance Traversal1 ~> Fold   where oTo = oTo @Traversal . oTo

-- AffineTraversal ---------------------------------------------------------

instance AffineTraversal ~> Traversal where
  oTo (AffineTraversal unzip) = Traversal $ \f s -> case unzip s of
                                  Left t            -> pure t
                                  Right (a, review) -> review <$> f a

instance AffineTraversal ~> AffineFold where
  oTo (AffineTraversal unzip) = AffineFold (either (const Nothing) (Just . fst) <$> unzip)

-- Transitive inclusions
instance AffineTraversal ~> Setter where oTo = oTo @Traversal . oTo
instance AffineTraversal ~> Fold   where oTo = oTo @Traversal . oTo

-- Traversal ---------------------------------------------------------------

instance Traversal ~> Setter where
  oTo (Traversal trav) = Setter (\f s -> runIdentity $ trav (Identity <$> f) s)

instance Traversal ~> Fold where
  oTo (Traversal trav) = Fold (\f -> getConst . trav (Const <$> f))

-- Glass -------------------------------------------------------------------

instance Glass ~> Kaleidoscope1 where
  oTo (Glass unzip) = Kaleidoscope1(\f ss@(s :| _) -> unzip s (\g -> f $ g <$> ss))

instance Glass ~> Setter where oTo = oTo @Kaleidoscope1 . oTo

-- Kaleidoscope -------------------------------------------------------------

instance Kaleidoscope ~> Kaleidoscope1 where
  oTo (Kaleidoscope agg) = Kaleidoscope1 (\f ss -> agg (f . NonEmpty.fromList) (toList ss))

instance Kaleidoscope ~> AffineKaleidoscope where
  oTo (Kaleidoscope agg) = AffineKaleidoscope (\f ss -> agg (f . listToMaybe) (toList ss))

instance Kaleidoscope ~> Setter where oTo = oTo @AffineKaleidoscope . oTo
instance Kaleidoscope ~> Review where oTo = oTo @AffineKaleidoscope . oTo

-- Kaleidoscope1 ------------------------------------------------------------

instance Kaleidoscope1 ~> Setter where
  oTo (Kaleidoscope1 agg) = Setter (\f s -> agg (f . NonEmpty.head) (s :| []))

-- AffineKaleidoscope -------------------------------------------------------

instance AffineKaleidoscope ~> Setter where
  oTo (AffineKaleidoscope agg) = Setter (\f s -> agg (f . fromJust) (Just s))

instance AffineKaleidoscope ~> Review where
  oTo (AffineKaleidoscope agg) = Review (\b -> agg (const b) Nothing)

-- Getter -------------------------------------------------------------------

instance Getter ~> Fold1 where
  oTo (Getter get) = Fold1 $ \f s -> f (get s)

instance Getter ~> AffineFold where
  oTo (Getter get) = AffineFold (Just . get)

-- Transitive inclusions
instance Getter ~> Fold where oTo = oTo @Fold1 . oTo

-- Fold1 --------------------------------------------------------------------

instance Fold1 ~> Fold where
  oTo (Fold1 fold) = Fold fold

-- AffineFold ---------------------------------------------------------------

instance AffineFold ~> Fold where
  oTo (AffineFold unzip) = Fold $ \ f -> maybe mempty f . unzip

-- Unknown -----------------------------------------------------------------

instance a ~> Unknown where oTo _ = Unknown

----------------------------------------------------------------------------
-------------------------------- MEETS -------------------------------------
----------------------------------------------------------------------------

type family (:\/:) (l :: * -> * -> * -> * -> *) (r :: * -> * -> * -> * -> *) :: * -> * -> * -> * -> * where
  o                  :\/: o                  = o
  Iso                :\/: Equality           = Iso
  Equality           :\/: r                  = r
  Iso                :\/: r                  = r
  Unknown            :\/: r                  = Unknown
  Lens               :\/: (AlgLens m)        = Lens
  Lens               :\/: Prism              = AffineTraversal
  Lens               :\/: (AlgPrism m)       = AffineTraversal
  Lens               :\/: Grate              = Glass
  Lens               :\/: Glass              = Glass
  Lens               :\/: AffineTraversal    = AffineTraversal
  Lens               :\/: Traversal1         = Traversal1
  Lens               :\/: Traversal          = Traversal
  Lens               :\/: AffineFold         = AffineFold
  Lens               :\/: Fold1              = Fold1
  Lens               :\/: Fold               = Fold
  Lens               :\/: Kaleidoscope       = Kaleidoscope1
  Lens               :\/: Kaleidoscope1      = Kaleidoscope1
  Lens               :\/: AffineKaleidoscope = Setter
  Lens               :\/: Setter             = Setter
  Lens               :\/: Getter             = Getter
  Lens               :\/: Review             = Unknown
  (AlgLens m)        :\/: r                  = Lens :\/: r
  Prism              :\/: (AlgPrism m)       = Prism
  Prism              :\/: Grate              = Setter
  Prism              :\/: Glass              = Setter
  Prism              :\/: AffineTraversal    = AffineTraversal
  Prism              :\/: Traversal1         = Traversal
  Prism              :\/: Traversal          = Traversal
  Prism              :\/: AffineFold         = Unknown
  Prism              :\/: Fold1              = Unknown
  Prism              :\/: Fold               = Unknown
  Prism              :\/: Kaleidoscope       = AffineKaleidoscope
  Prism              :\/: AffineKaleidoscope = AffineKaleidoscope
  Prism              :\/: Kaleidoscope1      = Setter
  Prism              :\/: Setter             = Setter
  Prism              :\/: Getter             = Unknown
  Prism              :\/: Review             = Review
  (AlgPrism c)       :\/: r                  = Prism :\/: r
  Grate              :\/: Glass              = Glass
  Grate              :\/: AffineTraversal    = Setter
  Grate              :\/: Traversal1         = Setter
  Grate              :\/: Traversal          = Setter
  Grate              :\/: AffineFold         = Unknown
  Grate              :\/: Fold1              = Unknown
  Grate              :\/: Fold               = Unknown
  Grate              :\/: Kaleidoscope       = Kaleidoscope
  Grate              :\/: Kaleidoscope1      = Kaleidoscope1
  Grate              :\/: AffineKaleidoscope = AffineKaleidoscope
  Grate              :\/: Setter             = Setter
  Grate              :\/: Getter             = Unknown
  Grate              :\/: Review             = Review
  Glass              :\/: AffineTraversal    = Setter
  Glass              :\/: Traversal1         = Setter
  Glass              :\/: Traversal          = Setter
  Glass              :\/: AffineFold         = Unknown
  Glass              :\/: Fold1              = Unknown
  Glass              :\/: Fold               = Unknown
  Glass              :\/: Kaleidoscope       = Kaleidoscope1
  Glass              :\/: Kaleidoscope1      = Kaleidoscope1
  Glass              :\/: AffineKaleidoscope = Setter
  Glass              :\/: Setter             = Setter
  Glass              :\/: Getter             = Unknown
  Glass              :\/: Review             = Unknown
  AffineTraversal    :\/: Traversal1         = Traversal
  AffineTraversal    :\/: Traversal          = Traversal
  AffineTraversal    :\/: AffineFold         = AffineFold
  AffineTraversal    :\/: Fold1              = Fold
  AffineTraversal    :\/: Fold               = Fold
  AffineTraversal    :\/: Kaleidoscope       = Setter
  AffineTraversal    :\/: Kaleidoscope1      = Setter
  AffineTraversal    :\/: AffineKaleidoscope = Setter
  AffineTraversal    :\/: Setter             = Setter
  AffineTraversal    :\/: Getter             = AffineFold
  AffineTraversal    :\/: Review             = Unknown
  Traversal1         :\/: Traversal          = Traversal
  Traversal1         :\/: AffineFold         = Fold
  Traversal1         :\/: Fold1              = Fold1
  Traversal1         :\/: Fold               = Fold
  Traversal1         :\/: Kaleidoscope       = Setter
  Traversal1         :\/: Kaleidoscope1      = Setter
  Traversal1         :\/: AffineKaleidoscope = Setter
  Traversal1         :\/: Setter             = Setter
  Traversal1         :\/: Getter             = Fold1
  Traversal1         :\/: Review             = Unknown
  Traversal          :\/: AffineFold         = Fold
  Traversal          :\/: Fold1              = Fold
  Traversal          :\/: Fold               = Fold
  Traversal          :\/: Kaleidoscope       = Setter
  Traversal          :\/: Kaleidoscope1      = Setter
  Traversal          :\/: AffineKaleidoscope = Setter
  Traversal          :\/: Setter             = Setter
  Traversal          :\/: Getter             = Fold
  Traversal          :\/: Review             = Unknown
  AffineFold         :\/: Fold1              = Fold
  AffineFold         :\/: Fold               = Fold
  AffineFold         :\/: Kaleidoscope       = Unknown
  AffineFold         :\/: Kaleidoscope1      = Unknown
  AffineFold         :\/: AffineKaleidoscope = Unknown
  AffineFold         :\/: Setter             = Unknown
  AffineFold         :\/: Getter             = AffineFold
  AffineFold         :\/: Review             = Unknown
  Fold1              :\/: Fold               = Fold
  Fold1              :\/: Kaleidoscope       = Unknown
  Fold1              :\/: Kaleidoscope1      = Unknown
  Fold1              :\/: AffineKaleidoscope = Unknown
  Fold1              :\/: Setter             = Unknown
  Fold1              :\/: Getter             = Fold1
  Fold1              :\/: Review             = Unknown
  Fold               :\/: Kaleidoscope       = Unknown
  Fold               :\/: Kaleidoscope1      = Unknown
  Fold               :\/: AffineKaleidoscope = Unknown
  Fold               :\/: Setter             = Unknown
  Fold               :\/: Getter             = Fold
  Fold               :\/: Review             = Unknown
  Kaleidoscope       :\/: Kaleidoscope1      = Kaleidoscope1
  Kaleidoscope       :\/: AffineKaleidoscope = AffineKaleidoscope
  Kaleidoscope       :\/: Setter             = Setter
  Kaleidoscope       :\/: Getter             = Unknown
  Kaleidoscope       :\/: Review             = Review
  Kaleidoscope1      :\/: AffineKaleidoscope = Setter
  Kaleidoscope1      :\/: Setter             = Setter
  Kaleidoscope1      :\/: Getter             = Unknown
  Kaleidoscope1      :\/: Review             = Unknown
  AffineKaleidoscope :\/: Setter             = Setter
  AffineKaleidoscope :\/: Getter             = Unknown
  AffineKaleidoscope :\/: Review             = Review
  Setter             :\/: Getter             = Unknown
  Setter             :\/: Review             = Unknown
  Getter             :\/: Review             = Unknown
  l                  :\/: r                  = r :\/: l

----------------------------------------------------------------------------
--------------------------- VIEW CONSTRAINT --------------------------------
----------------------------------------------------------------------------

----------------------------------------------------------------------------
----------------------------- COMPOSITIONS ---------------------------------
----------------------------------------------------------------------------

instance {-# OVERLAPPING #-} Composable' (Optic k) (Optic k) (Optic k) where
  Equality                 %? Equality                   = Equality
  Iso  get review          %? Iso get' review'           = Iso (get'.get) (review . review')
  Lens get put             %? Lens get' put'             = Lens (get'.get)      $ \s b -> put s (put' (get s)     b)
  AlgLens get put          %? AlgLens get' put'          = AlgLens (get' . get) $ \s b -> put s (put' (get <$> s) b)
  Prism matching review    %? Prism matching' review'    = Prism (\s -> case matching s of
                                                                          Left  t -> Left t
                                                                          Right x -> case matching' x of
                                                                            Left  y -> Left (review y)
                                                                            Right a -> Right a
                                                                  ) (review . review')
  AlgPrism matching review %? AlgPrism matching' review' = AlgPrism (\s -> case matching s of
                                                                            Left  t -> Left t
                                                                            Right x -> case matching' x of
                                                                              Left  y -> Left ( review <$> y)
                                                                              Right a -> Right a
                                                                    ) (review . review')
  Grate unzip              %? Grate unzip'               = Grate $ \f   -> unzip   (\g -> unzip'       (\g' -> f (g' . g)))
  Glass unzip              %? Glass unzip'               = Glass $ \s f -> unzip s (\g -> unzip' (g s) (\g' -> f (g' . g)))
  AffineTraversal unzip    %? AffineTraversal unzip'     = AffineTraversal $ \s -> case unzip s of
                                                                                    Left t -> Left t
                                                                                    Right (x, review) -> case unzip' x of
                                                                                      Left  y            -> Left (review y)
                                                                                      Right (a, review') -> Right (a, review . review')
  Traversal1 trav          %? Traversal1 trav'           = Traversal1         $ trav . trav'
  Traversal trav           %? Traversal trav'            = Traversal          $ trav . trav'
  AffineFold preview       %? AffineFold preview'        = AffineFold         $ preview >=> preview'
  Fold1 folding            %? Fold1 folding'             = Fold1              $ folding . folding'
  Fold folding             %? Fold folding'              = Fold               $ folding . folding'
  Getter get               %? Getter get'                = Getter             $ get' . get
  Kaleidoscope agg         %? Kaleidoscope agg'          = Kaleidoscope       $ agg . agg'
  Kaleidoscope1 agg        %? Kaleidoscope1 agg'         = Kaleidoscope1      $ agg . agg'
  AffineKaleidoscope agg   %? AffineKaleidoscope agg'    = AffineKaleidoscope $ agg . agg'
  Setter over              %? Setter over'               = Setter             $ over . over'
  Review review            %? Review review'             = Review             $ review . review'
  Unknown                  %? Unknown                    = Unknown

instance {-# OVERLAPPING #-} Composable (Optic k) (Optic k) (Optic k)

instance {-# OVERLAPPABLE #-} (o2 ~ o0 :\/: o1, o0 ~> o2, o1 ~> o2, Composable o2 o2 o2) => Composable o0 o1 o2 where
  f % g = oTo @o0 @o2 f % oTo @o1 @o2 g

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
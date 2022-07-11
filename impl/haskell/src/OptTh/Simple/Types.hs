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

----------------------------------------------------------------------------
------------------------ SIMPLE OPTICS TYPE ALIASES ------------------------
----------------------------------------------------------------------------

type Equality        = Optic K.Equality
type Iso             = Optic K.Iso 
type Setter          = Optic K.Setter
type Lens            = Optic K.Lens
type AlgLens m       = Optic (K.AlgLens m)
type AchromaticLens  = Optic K.AchromaticLens
type ClassifyingLens = Optic K.ClassifyingLens
type Prism           = Optic K.Prism 
type AlgPrism m      = Optic (K.AlgPrism m)
type Grate           = Optic K.Grate
type Glass           = Optic K.Glass
type Traversal       = Optic K.Traversal
type Traversal1      = Optic K.Traversal1
type AffineTraversal = Optic K.AffineTraversal
type Kaleidoscope    = Optic K.Kaleidoscope
type Getter          = Optic K.Getter
type Fold            = Optic K.Fold
type Fold1           = Optic K.Fold1
type AffineFold      = Optic K.AffineFold
type Review          = Optic K.Review
type Unknown         = Optic K.Unknown

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

-- Iso ---------------------------------------------------------------------

instance Monad m => Iso ~> AlgLens m where
  oTo (Iso get from) = AlgLens get (const from)

instance Comonad c => Iso ~> AlgPrism c where
  oTo (Iso get from) = AlgPrism (Right . get) from

instance Iso ~> Grate where
  oTo (Iso get from) = Grate $ \f -> from (f get)

-- Transitive inclusions
instance Iso ~> Lens            where oTo = oTo @(AlgLens  Identity) . oTo
instance Iso ~> Prism           where oTo = oTo @(AlgPrism Identity) . oTo
instance Iso ~> AffineTraversal where oTo = oTo @Lens . oTo
instance Iso ~> Traversal1      where oTo = oTo @Lens . oTo
instance Iso ~> Glass           where oTo = oTo @Lens . oTo
instance Iso ~> Traversal       where oTo = oTo @Lens . oTo
instance Iso ~> Setter          where oTo = oTo @Lens . oTo
instance Iso ~> Getter          where oTo = oTo @Lens . oTo
instance Iso ~> AffineFold      where oTo = oTo @Lens . oTo
instance Iso ~> Fold1           where oTo = oTo @Lens . oTo
instance Iso ~> Fold            where oTo = oTo @Lens . oTo
instance Iso ~> Review          where oTo = oTo @Prism . oTo
instance Iso ~> Kaleidoscope    where oTo = oTo @Grate . oTo

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

-- AchromaticLens ----------------------------------------------------------

instance AchromaticLens ~> Review where
  oTo (AlgLens get put) = Review (put Nothing)

-- ListLens ----------------------------------------------------------------

instance AlgLens [] ~> Kaleidoscope where
  oTo (AlgLens get put) = Kaleidoscope $ \f ss -> put ss ((f . fmap get) ss)

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
instance Lens ~> Traversal  where oTo = oTo @AffineTraversal . oTo
instance Lens ~> Setter     where oTo = oTo @AffineTraversal . oTo
instance Lens ~> Fold1      where oTo = oTo @Getter . oTo
instance Lens ~> Fold       where oTo = oTo @Getter . oTo
instance Lens ~> AffineFold where oTo = oTo @Getter . oTo

-- Prism -------------------------------------------------------------------

instance Prism ~> AffineTraversal where
  oTo (Prism preview review) = AffineTraversal (fmap (,review) <$> preview)

instance Prism ~> Review where
  oTo (Prism _ review) = Review review

-- Transitive inclusions
instance Prism ~> Traversal  where oTo = oTo @AffineTraversal . oTo
instance Prism ~> Setter     where oTo = oTo @AffineTraversal . oTo
instance Prism ~> AffineFold where oTo = oTo @AffineTraversal . oTo
instance Prism ~> Fold       where oTo = oTo @AffineTraversal . oTo

-- Grate -------------------------------------------------------------------

instance Grate ~> Glass where
  oTo (Grate unzip) = Glass $ const unzip

instance Grate ~> Review where
  oTo (Grate unzip) = Review (unzip . const)

instance Grate ~> Kaleidoscope where
  oTo (Grate unzip) = Kaleidoscope (\f s -> unzip $ \g -> f (g <$> s))

-- Transitive inclusions
instance Grate ~> Setter where oTo = oTo @Glass . oTo

-- Traversal1 --------------------------------------------------------------

instance Traversal1 ~> Traversal where
  oTo (Traversal1 trav) = Traversal trav

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

instance Glass ~> Setter where
  oTo (Glass unzip) = Setter (\f s -> unzip s (\g -> f $ g s))

-- Kaleidoscope -------------------------------------------------------------

instance Kaleidoscope ~> Setter where
  oTo (Kaleidoscope collect) = Setter (\f s -> collect (f . head) [s])

instance Kaleidoscope ~> Review where
  oTo (Kaleidoscope collect) = Review (\b -> collect (const b) [])

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
  Grate           :\/: Kaleidoscope    = Kaleidoscope
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
  Traversal1 trav          %? Traversal1 trav'           = Traversal1   $ trav . trav'
  Traversal trav           %? Traversal trav'            = Traversal    $ trav . trav'
  AffineFold preview       %? AffineFold preview'        = AffineFold   $ preview >=> preview'
  Fold1 folding            %? Fold1 folding'             = Fold1        $ folding . folding'
  Fold folding             %? Fold folding'              = Fold         $ folding . folding'
  Kaleidoscope collect     %? Kaleidoscope collect'      = Kaleidoscope $ collect . collect'
  Setter over              %? Setter over'               = Setter       $ over . over'
  Getter get               %? Getter get'                = Getter       $ get' . get
  Review review            %? Review review'             = Review       $ review . review'
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
module OptTh.Profunctor.Types where

import OptTh.Prelude
import qualified OptTh.Kinds.Optics as K
import OptTh.Common.Profunctor
import OptTh.Common.Staby
import Data.Kind (Constraint)

type family Constraints k p :: Constraint where
  Constraints K.Equality            p = ()
  Constraints K.Iso                 p = (Profunctor p)
  Constraints K.Lens                p = (Strong p)
  Constraints K.ReversedLens        p = (Costrong p)
  Constraints K.Prism               p = (Choice p)
  Constraints K.ReversedPrism       p = (Cochoice p)
  Constraints K.Grate               p = (Closed p)
  Constraints K.Glass               p = (Closed p, Strong p)
  Constraints K.Traversal           p = (Traversing p)
  Constraints K.Traversal1          p = (Traversing1 p)
  Constraints K.AffineTraversal     p = (Strong p, Choice p)
  Constraints K.Setter              p = (Mapping p)
  Constraints K.Getter              p = (Bicontravariant p, Cochoice p, Strong p)
  Constraints K.AffineFold          p = (Bicontravariant p, Cochoice p, Strong p, Choice p)
  Constraints K.Fold1               p = (Bicontravariant p, Cochoice p, Traversing1 p)
  Constraints K.Fold                p = (Bicontravariant p, Cochoice p, Traversing p)
  Constraints K.Review              p = (Bifunctor p, Choice p, Costrong p, Closed p)
  Constraints K.Kaleidoscope        p = (Aggregating p)
  Constraints K.Kaleidoscope1       p = (Aggregating1 p)
  Constraints K.AffineKaleidoscope  p = (AffineAggregating p)
  Constraints K.Unknown             p = (Bicontravariant p, Bifunctor p, Cochoice p)

type Optic  (k :: K.OpticKind) s t a b = forall p. Constraints k p => p a b -> p s t
type Optic' (k :: K.OpticKind) s a     = Optic k s s a a

type Equality            s t a b = Optic K.Equality            s t a b
type Iso                 s t a b = Optic K.Iso                 s t a b
type Lens                s t a b = Optic K.Lens                s t a b
type ReversedLens        s t a b = Optic K.ReversedLens        s t a b
type Prism               s t a b = Optic K.Prism               s t a b
type ReversedPrism       s t a b = Optic K.ReversedPrism       s t a b
type Grate               s t a b = Optic K.Grate               s t a b
type Glass               s t a b = Optic K.Glass               s t a b
type Traversal           s t a b = Optic K.Traversal           s t a b
type Traversal1          s t a b = Optic K.Traversal1          s t a b
type AffineTraversal     s t a b = Optic K.AffineTraversal     s t a b
type Setter              s t a b = Optic K.Setter              s t a b
type Getter              s t a b = Optic K.Getter              s t a b
type AffineFold          s t a b = Optic K.AffineFold          s t a b
type Fold1               s t a b = Optic K.Fold1               s t a b
type Fold                s t a b = Optic K.Fold                s t a b
type Review              s t a b = Optic K.Review              s t a b
type Kaleidoscope        s t a b = Optic K.Kaleidoscope        s t a b
type Kaleidoscope1       s t a b = Optic K.Kaleidoscope1       s t a b
type AffineKaleidoscope  s t a b = Optic K.AffineKaleidoscope  s t a b

equality :: forall x y. Equality x y x y
equality = id

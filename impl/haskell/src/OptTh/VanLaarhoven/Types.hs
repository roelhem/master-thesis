module OptTh.VanLaarhoven.Types where

import OptTh.Prelude
import qualified OptTh.Kinds.Optics as K
import OptTh.Common.Functor (Settable)

----------------------------------------------------------------------------
--------------------- LENS-LIKE VAN-LAARHOVEN OPTICS -----------------------
----------------------------------------------------------------------------

type VL       g s t a b = (  a -> g b) -> (  s -> g t)
type CoVL   f   s t a b = (f a ->   b) -> (f s ->   t)
type ProVL  f g s t a b = (f a -> g b) -> (f s -> g t)
type VL'      g s   a   = VL      g s s a a
type CoVL'  f   s   a   = CoVL  f   s s a a
type ProVL' f g s   a   = ProVL f g s s a a

type family VLConstraint (k :: K.OpticKind) f where
  VLConstraint K.Lens f            = (Functor f)
  VLConstraint K.AffineTraversal f = (Functor f, Pointed f)
  VLConstraint K.Traversal1 f      = (Apply f)
  VLConstraint K.Traversal f       = (Pointed f, Apply f, Applicative f)
  VLConstraint K.Setter f          = (Settable f, Copointed f)
  VLConstraint K.Getter f          = (Contravariant f, VLConstraint K.Lens f)
  VLConstraint K.AffineFold f      = (Contravariant f, VLConstraint K.AffineTraversal f)
  VLConstraint K.Fold1 f           = (Contravariant f, VLConstraint K.Traversal1 f)
  VLConstraint K.Fold f            = (Contravariant f, VLConstraint K.Traversal f)

type VLOptic  (k :: K.OpticKind) s t a b = forall f. (VLConstraint k f) => (a -> f b) -> s -> f t
type VLOptic' (k :: K.OpticKind) s a     = VLOptic (k :: K.OpticKind) s s a a

----------------------------------------------------------------------------
--------------------- GRATE-LIKE VAN-LAARHOVEN OPTICS ----------------------
----------------------------------------------------------------------------

type family CoVLConstraint (k :: K.OpticKind) g where
  CoVLConstraint K.Grate g         = (Functor g)
  CoVLConstraint K.Review g        = (Functor g, Contravariant g)
  CoVLConstraint K.Setter g        = (Settable g, Copointed g)

type CoVLOptic  (k :: K.OpticKind) s t a b = forall g. (CoVLConstraint k g) => (g a -> b) -> g s -> t
type CoVLOptic' (k :: K.OpticKind) s a     = CoVLOptic k s s a a

----------------------------------------------------------------------------
------------------------------ TYPE ALIASES --------------------------------
----------------------------------------------------------------------------

type Lens             s t a b = VLOptic    K.Lens             s t a b
type AffineTraversal  s t a b = VLOptic    K.AffineTraversal  s t a b
type Traversal1       s t a b = VLOptic    K.Traversal1       s t a b
type Traversal        s t a b = VLOptic    K.Traversal        s t a b
type Setter           s t a b = VLOptic    K.Setter           s t a b
type Getter           s t a b = VLOptic    K.Getter           s t a b
type AffineFold       s t a b = VLOptic    K.AffineFold       s t a b
type Fold1            s t a b = VLOptic    K.Fold1            s t a b
type Fold             s t a b = VLOptic    K.Fold             s t a b

type Grate            s t a b = CoVLOptic  K.Grate            s t a b
type Review           s t a b = CoVLOptic  K.Review           s t a b
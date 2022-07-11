module OptTh.Profunctor.Types where
  
import qualified OptTh.Kinds.Optics as K
import OptTh.Common.Profunctor

type family Constraint k p where
  Constraint K.Iso p             = (Profunctor p)
  Constraint K.Lens p            = (Strong p)
  Constraint K.ReversedLens p    = (Costrong p)
  Constraint K.Prism p           = (Choice p)
  Constraint K.ReversedPrism p   = (Cochoice p)
  Constraint K.Grate p           = (Closed p)
  Constraint K.Glass p           = (Closed p, Strong p)
  Constraint K.Traversal p       = (Traversing p)
  Constraint K.Traversal1 p      = (Traversing1 p)
  Constraint K.AffineTraversal p = (AffineTraversing p)
  Constraint K.Setter p          = (Mapping p)
  Constraint K.Getter p          = (Bicontravariant p, Cochoice p, Strong p)
  Constraint K.AffineFold p      = (Bicontravariant p, Cochoice p, AffineTraversing p)
  Constraint K.Fold1 p           = (Bicontravariant p, Cochoice p, Traversing1 p)
  Constraint K.Fold p            = (Bicontravariant p, Cochoice p, Traversing p)
  Constraint K.Review p          = (Bifunctor p, Choice p, Costrong p)
  Constraint K.Kaleidoscope p    = (Classifying p)
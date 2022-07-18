module OptTh.Profunctor.Convert where

import OptTh.Prelude
import OptTh.Common.Profunctor
import OptTh.Profunctor.Types
import qualified OptTh.Simple.Types as S
import OptTh.Common.Functor
import OptTh.Common.Cardinalities
import Data.Pointed ()
import Data.Functor.Apply (WrappedApplicative(..))
import Data.Foldable (Foldable(..))

toProf :: S.Optic k s t a b -> Optic k s t a b
toProf S.Equality                    = id
toProf (S.Iso view review)           = dimap view review
toProf (S.Lens view put)             = dimap (\s -> (s, view s)) (uncurry put) . second'
toProf (S.AlgLens view put)          = undefined
toProf (S.Prism matching review)     = dimap matching (either id review) . right'
toProf (S.AlgPrism matching review)  = undefined
toProf (S.Grate grate)               = dimap (flip ($)) grate . closed
toProf (S.Glass rezip)               = dimap (\s -> (s, ($ s))) (uncurry rezip) . second' . closed 
toProf (S.AffineTraversal unzip)     = wanderAff (\ f s -> either point (\(a, bf) -> bf <$> f a) (unzip s))
toProf (S.Traversal1 trav)           = wander1 trav
toProf (S.Traversal trav)            = wander trav
toProf (S.Kaleidoscope agg)          = aggregating    (generalizeAgg @"*" agg)
toProf (S.Kaleidoscope1 agg)         = aggregating1   (generalizeAgg @"+" agg)
toProf (S.AffineKaleidoscope agg)    = aggregatingAff (generalizeAgg @"?" agg)
toProf (S.Setter over)               = roam over
toProf (S.Getter view)               = dimap view undefined -- TODO: Remove Undefined
toProf (S.Fold folding)              = contrabimap (folding $ cpoint @"*") (const []) . traverse' -- TODO: Remove Undefined
toProf (S.Fold1 folding)             = dimap (folding $ cpoint @"+") undefined . traverse1' -- TODO: Remove Undefined
toProf (S.AffineFold folding)        = contrabimap folding undefined . traverseAff' -- TODO: Remove Undefined
toProf (S.Review review)             = dimap undefined review -- TODO: Remove Undefined
toProf S.Unknown                     = unright . bimap (const $ Left ()) (const $ Left ())
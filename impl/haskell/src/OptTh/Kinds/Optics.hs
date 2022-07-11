module OptTh.Kinds.Optics where

import OptTh.Prelude
import qualified Optics as O
import OptTh.Kinds.TH (kindAliasesFromOpticsLib, customKindDeclarations)
import Data.Kind (Type)

----------------------------------------------------------------------------
----------------------------- OPTIC KINDS ----------------------------------
----------------------------------------------------------------------------

kindAliasesFromOpticsLib
customKindDeclarations

type OpticKind = O.OpticKind

data AlgLens  (m :: * -> *) :: OpticKind
data AlgPrism (m :: * -> *) :: OpticKind

----------------------------------------------------------------------------
---------------------------- KIND ALIASES ----------------------------------
----------------------------------------------------------------------------

type AchromaticLens = AlgLens Maybe
type ClassifyingLens = AlgLens []

-- instance O.JoinKinds Grate Lens Glass where
  
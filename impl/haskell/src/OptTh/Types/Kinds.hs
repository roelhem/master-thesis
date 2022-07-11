module OptTh.Types.Kinds where

import qualified Optics as O
import OptTh.Types.TH (kindAliasesFromOpticsLib, customKindDeclarations)
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
----------------------------- JOIN KINDS -----------------------------------
----------------------------------------------------------------------------

-- instance O.JoinKinds Grate Lens Glass where
  
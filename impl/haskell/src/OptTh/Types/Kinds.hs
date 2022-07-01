{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

----------------------------------------------------------------------------
----------------------------- JOIN KINDS -----------------------------------
----------------------------------------------------------------------------

-- instance O.JoinKinds Grate Lens Glass where
  
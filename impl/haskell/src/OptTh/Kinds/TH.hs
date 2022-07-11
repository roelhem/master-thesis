module OptTh.Kinds.TH where

import OptTh.Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Optics as O
import Data.List (isPrefixOf)
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity)

data KindDef = KindDef { name :: String
                       , autoDefine :: Bool
                       , inOpticsLib :: Bool
                       }

kindDefs :: [KindDef]
kindDefs = [ KindDef "Equality"        True  False
           , KindDef "Iso"             True  True
           , KindDef "Lens"            True  True
           , KindDef "ReversedLens"    True  True
           , KindDef "AlgLens"         False False
           , KindDef "Prism"           True  True
           , KindDef "ReversedPrism"   True  True
           , KindDef "AlgPrism"        False False
           , KindDef "Traversal"       True  True
           , KindDef "Traversal1"      True  False
           , KindDef "AffineTraversal" True  True
           , KindDef "Fold"            True  True
           , KindDef "Fold1"           True  False
           , KindDef "AffineFold"      True  True
           , KindDef "Setter"          True  True
           , KindDef "Getter"          True  True
           , KindDef "Review"          True  True
           , KindDef "Grate"           True  False
           , KindDef "Glass"           True  False
           , KindDef "Kaleidoscope"    True  False
           , KindDef "Unknown"         True  False
           ]

----------------------------------------------------------------------------
----------------------------- KIND ALIASES ---------------------------------
----------------------------------------------------------------------------

kindAlias :: String -> Q Dec
kindAlias n = tySynD name [] (conT $ mkName (prefix ++ n))
  where name = mkName n
        prefix | "A" `isPrefixOf` n || "I" `isPrefixOf` n = "O.An_"
               | otherwise          = "O.A_"

kindAliases :: [String] -> Q [Dec]
kindAliases = mapM kindAlias

kindDeclaration :: String -> Q Dec
kindDeclaration n = pure (DataD [] (mkName n) [] (Just (ConT (mkName "O.OpticKind"))) [] [])

kindDeclarations :: [String] -> Q [Dec]
kindDeclarations = mapM kindDeclaration

kindAliasesFromOpticsLib :: Q [Dec]
kindAliasesFromOpticsLib = kindAliases $ name <$> filter inOpticsLib kindDefs

customKindDeclarations :: Q [Dec]
customKindDeclarations = kindDeclarations $ name <$> filter (\x -> not (inOpticsLib x) && autoDefine x) kindDefs

----------------------------------------------------------------------------
----------------------------- OPTIC DATATYPES ------------------------------
----------------------------------------------------------------------------

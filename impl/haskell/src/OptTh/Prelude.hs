module OptTh.Prelude ( module Prelude
                     , module Control.Category
                     , module Data.Functor.Identity
                     , module Data.Functor.Compose
                     , module Data.Tuple
                     , module Data.Void
                     , module OptTh.Common
                     , module GHC.Base
                     , module Data.Default
                     ) where

import Prelude hiding ( (.)
                      , id
                      )
import Control.Category ((.), id)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))
import Data.Void (Void(..), absurd, vacuous)
import Data.Tuple (swap)
import GHC.Base (NonEmpty (..))
import Data.Default (Default (..))

import OptTh.Common ( Apply
                    , Traversable1(..)
                    , Contravariant
                    , Pointed
                    , Copointed
                    , Foldable1(..)
                    , Composable'(..)
                    , Composable(..)
                    , type (~>) (..)
                    )

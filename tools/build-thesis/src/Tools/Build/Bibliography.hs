{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module Tools.Build.Bibliography where
import Data.Map (Map)
import Control.Lens
import GHC.Generics
import Data.Generics.Sum
import Data.Generics.Product
import Network.Wreq

data BibTexEntry = BibTexEntry { entryType :: String,
                                 citekey   :: String,
                                 fields    :: Map String String
                               } deriving (Show, Generic)

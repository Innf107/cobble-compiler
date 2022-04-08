module Cobble.Interface where

import Cobble.Prelude
import Cobble.Types

import Data.Binary

data Interface = Interface {
    interfaceModName :: Text
,   interfaceModSig :: ModSig
} deriving (Show, Eq, Generic, Data)

instance Binary Interface



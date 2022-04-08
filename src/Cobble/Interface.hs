module Cobble.Interface where

import Cobble.Prelude
import Cobble.Types

import Data.Binary

newtype Interface = Interface {
    interfaceModSig :: ModSig
} deriving (Show, Eq, Generic, Data)

instance Binary Interface



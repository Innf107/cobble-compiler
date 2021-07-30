module Language.Cobble.CPS.TopLevel.Types where

import Language.Cobble.Prelude
import Language.Cobble.Shared

data TL = LetF QualifiedName [QualifiedName] TLC TL
        | LetC [QualifiedName] TLC TL
        | C TLC
        deriving (Show, Eq, Generic, Data)

data TLC = Let QualifiedName TLExp TLC
         | App QualifiedName [QualifiedName]
         deriving (Show, Eq, Generic, Data)

data TLExp = IntLit Int
           | Var QualifiedName
           | Halt
           | Tuple [QualifiedName]
           | Select Int QualifiedName
           deriving (Show, Eq, Generic, Data)

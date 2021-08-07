module Language.Cobble.MCAsm.Types where

import Language.Cobble.Prelude

import Language.Cobble.Shared
import Language.Cobble.Codegen.Common

import GHC.Show qualified as S

import Data.Data

import Data.Generics.Uniplate.Data

-- Can use an unlimited amount of Registers

data Register = Reg QualifiedName -- Reg x        -> $x
              | SpecialReg Text   -- SpecialReg x -> %x
              deriving (Show, Eq, Generic, Data)

data Block = Block QualifiedName [Instruction] deriving (Show, Eq, Generic, Data)

data Instruction = 
      Move Register Register
    | MoveLit Register Int
                 
    | Add Register Register
    | AddLit Register Int
    | Sub Register Register
    | SubLit Register Int
    | Mul Register Register
    | Div Register Register
    | Mod Register Register
    
    | Min Register Register
    | Max Register Register
    
    | Call QualifiedName
    | ICall Register
    | LoadFunctionAddress Register QualifiedName

    | CallInRange Register Range QualifiedName
    | CallEQ      Register Register QualifiedName
    | CallLT      Register Register QualifiedName
    | CallGT      Register Register QualifiedName
    | CallLE      Register Register QualifiedName
    | CallGE      Register Register QualifiedName

    | Malloc Register Int
    | Select Register Register Int
    | Store  Register Register Int
    deriving (Show, Eq, Generic, Data)


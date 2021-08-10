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

{- [Note: Exec Instructions]
Unfortunately, the varius Exec* instructions are allowed by their types to accept
instructions, that cannot be compiled to a single command and therefore have to panic the compiler.
Ideally, Instructions would be a GADT with a parameter of type InstructionUsage with @data InstructionUsage = Compile | Exec@.
Now, all instructions that can be compiled to a single command would have return type @Instruction u@
while all others would have return type @Instruction Compile@.
Now, the Exec instructions could require their parameters to have type @Instructions Exec@ and would thus exclude all invalid commands.

Unfortunately, this is not possible, since GHC cannot derive @Generic@ for GADTs and thus, 
Data and Uniplate would be inaccessible :( 
-}

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

    | ExecInRange Register Range    Instruction
    | ExecEQ      Register Register Instruction
    | ExecLT      Register Register Instruction
    | ExecGT      Register Register Instruction
    | ExecLE      Register Register Instruction
    | ExecGE      Register Register Instruction

    | Malloc Register Int
    | Select Register Register Int
    | Store  Register Register Int
    deriving (Show, Eq, Generic, Data)


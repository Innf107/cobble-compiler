module Language.Cobble.LC.Types where

import Language.Cobble.Prelude

import Language.Cobble.Shared
import Language.Cobble.Codegen.Common

import Data.Data

type LCCon = QualifiedName

data LCDef = LCDef QualifiedName LCExpr deriving (Show, Eq)

data LCExpr = Var QualifiedName
            | Lambda QualifiedName LCExpr
            | App LCExpr LCExpr
            | IntLit Int
           --  | Con LCCon [LCExpr] -- todo: represent variants as records with an additional tag field instead?
           --  | Decon LCCon LCExpr
          --   | Switch LCExpr [(LCCon, LCExpr)] -- Used for variants and case exprs (NYI)
            | Tuple [LCExpr]
            | Select Int LCExpr
            deriving (Show, Eq, Generic, Data)




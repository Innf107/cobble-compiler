module Language.Cobble.LC.Types where

import Language.Cobble.Prelude

import Language.Cobble.Shared

type LCVar = QualifiedName

type LCCon = QualifiedName

data LCDef = LCDef QualifiedName LCExpr deriving (Show, Eq)

data LCExpr = Var LCVar
            | Lambda LCVar LCExpr
            | Fix LCVar LCExpr
            | App LCExpr LCExpr
            | IntLit Int
           --  | Con LCCon [LCExpr]
           --  | Decon LCCon LCExpr
          --   | Switch LCExpr [(LCCon, LCExpr)] -- Used for variants and case exprs (NYI)
            | Tuple [LCExpr]
            | SelectTuple Int LCExpr
            deriving (Show, Eq)




module Language.Cobble.LC.Types where

import Language.Cobble.Prelude

import Language.Cobble.Types.QualifiedName
import Language.Cobble.Codegen.Common
import Language.Cobble.Codegen.PrimOps

import Data.Data

type LCCon = QualifiedName

data LCDef = LCDef QualifiedName LCExpr deriving (Show, Eq)

data LCExpr = Var QualifiedName
            | Lambda QualifiedName LCExpr
            | App LCExpr LCExpr
            | Let QualifiedName LCExpr LCExpr
            | LetRec QualifiedName QualifiedName LCExpr LCExpr
            | IntLit Int
            | Tuple [LCExpr]
            | Variant (QualifiedName, Int) [LCExpr]
            | Select Int LCExpr
            | If LCExpr LCExpr LCExpr
            | PrimOp PrimOp [LCExpr]
            deriving (Show, Eq, Generic, Data)




module Language.Cobble.CPS.Types where

import Language.Cobble.Prelude 
import Language.Cobble.Shared 
 
type CPSVar = QualifiedName
  
type PrimopID = QualifiedName

data CPSDef = CPSDef QualifiedName CPSExpr

data CPSExpr = Var CPSVar
             | Lambda CPSVar CPSExpr
             | App CPSExpr CPSExpr
             | IntLit Int
             | Tuple [CPSExpr]
             | SelectTuple Int CPSExpr
             deriving (Show, Eq)
             
module Language.Cobble.Types  
    (
      module Export
    , NameSpace
    , HasType (..)
    )
    where

import Language.Cobble.Types.AST as Export
import Language.Cobble.Types.AST.SolveModules as Export
import Language.Cobble.Types.AST.QualifyNames as Export
import Language.Cobble.Types.AST.Typecheck as Export
import Language.Cobble.Types.AST.Codegen as Export
import Language.Cobble.Shared as Export
import Language.Cobble.MCAsm.Types as Export (Objective(..)) 

import Language.Cobble.Prelude

type NameSpace = Text


class HasType t p | t -> p where
    getType :: t -> Type p

instance HasType (Expr 'Codegen) 'Codegen where
    getType = \case
        FCall x _ _ _ -> x
        Var x _ _ -> x
        IntLit () _ _ -> intT
        If _ _ _ th _ -> getType th
        UnitLit _ -> unitT
        Let _ _ _ _ e -> getType e
        ExprX v _ -> absurd v
        
instance HasType (Type p) p where
    getType = id
        
module Language.Cobble.Types  
    (
      module Export
    , NameSpace
    , HasType (..)
    )
    where

import Language.Cobble.Types.Lens as Export
import Language.Cobble.Types.AST as Export
import Language.Cobble.Types.AST.SolveModules as Export
import Language.Cobble.Types.AST.QualifyNames as Export
import Language.Cobble.Types.AST.SemAnalysis as Export
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
        FCall (Ext t) _ _ _                 -> t
        Var (Ext t) _ _                     -> t
        IntLit _ _ _                        -> intT
        If _ _ _ th _                       -> getType th
        UnitLit _                           -> unitT
        Let _ _ _ b                         -> getType b
        StructConstruct (Ext (_, t)) _ _ _  -> t
        StructAccess (Ext (_, t)) _ _ _     -> t
        ExprX v _                           -> absurd v
        
instance HasType (Type p) p where
    getType = id
        
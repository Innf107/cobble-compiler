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
import Language.Cobble.Types.Instances as Export
import Language.Cobble.Types.QualifiedName as Export
import Language.Cobble.Types.LexInfo as Export

import Language.Cobble.Prelude

type NameSpace = Text


class HasType t where
    getType :: t -> Type


instance HasType (Expr 'Codegen) where
    getType = \case
        App t _ _ _                         -> t
        Var (t, _) _ _                      -> t
        Ascription t _ _ _                  -> t
        VariantConstr (t,_,_) _ _           -> t
        Case t _ _ _                        -> t
        IntLit _ _ _                        -> intT
        If _ _ _ th _                       -> getType th
        UnitLit _                           -> unitT
        Let _ _ _ b                         -> getType b
        Lambda t _ _ _                      -> t

instance HasType (Decl Codegen) where
    getType (Decl (t, _) _ _ _) = t

instance HasType (Pattern Codegen) where
    getType (IntP () n) = intT
    getType (VarP ty _) = ty
    getType (ConstrP (ty, _) _ _) = ty

instance HasType Type where
    getType = id

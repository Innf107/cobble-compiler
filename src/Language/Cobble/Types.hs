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
import Language.Cobble.Types.AST.PostProcess as Export
import Language.Cobble.Types.AST.Codegen as Export
import Language.Cobble.Types.Instances as Export
import Language.Cobble.Types.QualifiedName as Export
import Language.Cobble.Types.LexInfo as Export

import Language.Cobble.Prelude

type NameSpace = Text


class HasType t p | t -> p where
    getType :: t -> Type p

instance HasType (Expr 'PostProcess) 'PostProcess where
    getType = \case
        FCall t _ _ _                       -> t
        Var (t, _) _ _                      -> t
        VariantConstr (t,_,_) _ _           -> t
        Case t _ _ _                        -> t
        IntLit _ _ _                        -> intT
        If _ _ _ th _                       -> getType th
        UnitLit _                           -> unitT
        Let _ _ _ b                         -> getType b
        StructConstruct (_, t) _ _ _        -> t
        StructAccess (_, _, t) _ _ _        -> t


instance HasType (Expr 'Codegen) 'Codegen where
    getType = \case
        FCall t _ _ _                       -> t
        Var (t, _) _ _                      -> t
        VariantConstr (t,_,_) _ _           -> t
        Case t _ _ _                        -> t
        IntLit _ _ _                        -> intT
        If _ _ _ th _                       -> getType th
        UnitLit _                           -> unitT
        Let _ _ _ b                         -> getType b
        StructConstruct (_, t) _ _ _        -> t
        StructAccess (_, t) _ _ _           -> t

instance HasType (Decl PostProcess) PostProcess where
    getType (Decl (t, _) _ _ _) = t

instance HasType (Pattern PostProcess) PostProcess where
    getType (IntP () n) = intT
    getType (VarP ty _) = ty
    getType (ConstrP ty _ _) = ty

instance HasType (Type p) p where
    getType = id

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
    setType :: Type -> t -> t


instance HasType (Expr 'Codegen) where
    getType = \case
        App t _ _ _                         -> t
        Var (t, _) _ _                      -> t
        VariantConstr (t,_,_) _ _           -> t
        Case t _ _ _                        -> t
        IntLit _ _ _                        -> intT
        If _ _ _ th _                       -> getType th
        UnitLit _                           -> unitT
        Let _ _ _ b                         -> getType b
        Lambda (t, _) _ _ _                 -> t
        TyAbs _ tv e                        -> TForall [tv] (getType e)
        TyApp _ targ e                      -> getType e -- TODO: Should apply arg type
    setType t = \case
        App _ x y z                         -> App t x y z
        Var (_, x) y z                      -> Var (t, x) y z
        VariantConstr (_,x,y) z w           -> VariantConstr (t, x, y) z w
        Case _ x y z                        -> Case t x y z
        i@IntLit{}                          -> i
        If x y z th el                      -> If x y z (setType t th) (setType t el)
        u@UnitLit{}                         -> u
        Let x y z b                         -> Let x y z (setType t b)
        Lambda (_, x) y z w                 -> Lambda (t, x) y z w
        TyAbs li ty e                       -> TyAbs li ty (setType t e)
        TyApp li targ e                     -> TyApp li targ (setType t e)

instance HasType (Decl Codegen) where
    getType (Decl (t, _) _ _ _)   = t
    setType t (Decl (_, x) y z w) = Decl (t, x) y z w

instance HasType (Pattern Codegen) where
    getType (IntP () n)           = intT
    getType (VarP ty _)           = ty
    getType (ConstrP (ty, _) _ _) = ty

    setType _ i@IntP{}             = i
    setType t (VarP _ x)           = VarP t x
    setType t (ConstrP (_, x) y z) = ConstrP (t, x) y z

instance HasType Type where
    getType     = id
    setType t _ = t

module Cobble.Types  
    (
      module Export
    , NameSpace
    , HasType (..)
    )
    where

import Cobble.Types.Lens as Export
import Cobble.Types.AST as Export
import Cobble.Types.AST.SolveModules as Export
import Cobble.Types.AST.QualifyNames as Export
import Cobble.Types.AST.SemAnalysis as Export
import Cobble.Types.AST.Typecheck as Export
import Cobble.Types.AST.Codegen as Export
import Cobble.Types.Instances as Export
import Cobble.Types.QualifiedName as Export
import Cobble.Types.LexInfo as Export

import Cobble.Prelude

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
        Let _ _ _ b                         -> getType b
        Lambda (t, _, _) _ _ _              -> t
        TyAbs _ tv e                        -> TForall [tv] (getType e)
        TyApp _ targ e                      -> getType e -- TODO: Should apply arg type
        DictAbs _ _ c e                     -> TConstraint c (getType e)
        DictVarApp _ e _                    -> getType e -- TODO: Should apply dictionary
        DictApp _ e _                       -> getType e -- TODO: Should apply dictionary
    setType t = \case
        App _ x y z                         -> App t x y z
        Var (_, x) y z                      -> Var (t, x) y z
        VariantConstr (_,x,y) z w           -> VariantConstr (t, x, y) z w
        Case _ x y z                        -> Case t x y z
        i@IntLit{}                          -> i
        If x y z th el                      -> If x y z (setType t th) (setType t el)
        Let x y z b                         -> Let x y z (setType t b)
        Lambda (_, x, a) y z w              -> Lambda (t, x, a) y z w
        TyAbs li ty e                       -> TyAbs li ty (setType t e)
        TyApp li targ e                     -> TyApp li targ (setType t e)
        DictAbs li x c e                    -> DictAbs li x c (setType t e)
        DictVarApp li e arg                 -> DictVarApp li (setType t e) arg
        DictApp li e arg                    -> DictVarApp li (setType t e) arg

instance HasType (Decl Codegen) where
    getType (Decl (t, _) _ _ _)   = t
    setType t (Decl (_, x) y z w) = Decl (t, x) y z w

instance HasType (Pattern Codegen) where
    getType (IntP () n)              = intT
    getType (VarP ty _)              = ty
    getType (ConstrP (ty, _, _) _ _) = ty
    getType (WildcardP ty)           = ty
    getType (OrP ty _)               = ty

    setType _ i@IntP{}                = i
    setType t (VarP _ x)              = VarP t x
    setType t (ConstrP (_, x, w) y z) = ConstrP (t, x, w) y z
    setType t (WildcardP _)           = WildcardP t
    setType t (OrP _ pats)            = OrP t pats

instance HasType Type where
    getType     = id
    setType t _ = t

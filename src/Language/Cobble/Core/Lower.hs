{-# LANGUAGE OverloadedLists #-}
module Language.Cobble.Core.Lower where

import Language.Cobble.Prelude

import Language.Cobble.Types (QualifiedName)
import Language.Cobble.Types qualified as C

import Language.Cobble.Core.Types qualified as F
import Language.Cobble.Util.Bitraversable (secondM)

import Language.Cobble.Util.Polysemy.Fresh

type CExpr = C.Expr C.Codegen
type CStatement = C.Statement C.Codegen
type CModule = C.Module C.Codegen

lower :: Members '[Fresh Text QualifiedName] r => CModule -> Sem r [F.Decl]
lower (C.Module _deps mname sts) = lowerStmnts sts

lowerStmnts :: Members '[Fresh Text QualifiedName] r => [CStatement] -> Sem r [F.Decl]
lowerStmnts [] = pure []
lowerStmnts (C.Def _ _ (C.Decl _ x xs e) ty : sts) = (:)
    <$> do
            xs' <- traverse (secondM lowerType) xs
            e' <- lowerExpr e
            ty' <- lowerType ty
            pure $ F.Def x ty' (foldr (uncurry F.Abs) e' xs')
    <*> lowerStmnts sts
lowerStmnts (C.Import{} : sts) = lowerStmnts sts
lowerStmnts (C.DefClass{} : sts) = undefined
lowerStmnts (C.DefInstance{} : sts) = undefined
lowerStmnts (C.DefVariant _ _ x args clauses : sts) = do
    def <- F.DefVariant x 
            <$> traverse (\(C.MkTVar x k) -> (x,) <$> lowerKind k) args
            <*> traverse (\(x, tys, _) -> (x,) <$> traverse lowerType tys) clauses
    (def:) <$> lowerStmnts sts

lowerExpr :: Members '[Fresh Text QualifiedName] r => CExpr -> Sem r F.Expr
lowerExpr (C.VariantConstr (_ty, constrTy, ix) _ x) = lowerType constrTy >>= \ty -> lowerVariantConstr ty ix x [] []
lowerExpr (C.App ty _ e1 e2) = F.App
                               <$> lowerExpr e1
                               <*> lowerExpr e2
lowerExpr (C.IntLit () _ n) = pure $ F.IntLit n
lowerExpr (C.UnitLit li) = pure $ F.UnitLit
lowerExpr (C.If ty _ c th el) = F.If 
                                <$> lowerExpr c
                                <*> lowerExpr th
                                <*> lowerExpr el
lowerExpr (C.Let () _ (C.Decl (ty, _) f xs e1) e2) = do
    e1' <- lowerExpr e1
    F.Let f
        <$> lowerType ty
        <*> (foldrM (\(x, t) r -> F.Abs x <$> lowerType t <*> pure r) e1' xs)
        <*> lowerExpr e2
lowerExpr (C.Var _ _ x) = pure $ F.Var x
lowerExpr (C.Ascription _ _ e ty) = lowerExpr e -- Ascriptions are subsumed (hah) by type applications.
lowerExpr C.Case{} = undefined
lowerExpr (C.Lambda (ty, argTy) _ x e) = F.Abs x <$> lowerType argTy <*> lowerExpr e
lowerExpr (C.TyApp _ ty e) = F.TyApp <$> lowerExpr e <*> lowerType ty
lowerExpr (C.TyAbs _ (C.MkTVar tvName tvKind) e) = F.TyAbs tvName <$> lowerKind tvKind <*> lowerExpr e

lowerVariantConstr :: Members '[Fresh Text QualifiedName] r => F.Type -> Int -> QualifiedName -> Seq F.Type -> Seq F.Expr -> Sem r F.Expr
lowerVariantConstr (F.TForall a k ty) i constrName tyArgs valArgs = do
    a' <- freshVar (C.originalName a)
    F.TyAbs a' k <$> lowerVariantConstr ty i constrName (F.TVar a' k <| tyArgs) valArgs
lowerVariantConstr (F.TFun t1 t2) i constrName tyArgs valArgs = do
    x <- freshVar "x"
    F.Abs x t1 <$> lowerVariantConstr t2 i constrName tyArgs (F.Var x <| valArgs)
lowerVariantConstr ty i constrName tyArgs valArgs = do
    pure $ F.VariantConstr constrName i tyArgs valArgs

lowerType :: C.Type -> Sem r F.Type
lowerType (C.TVar (C.MkTVar tvName tvKind)) = F.TVar tvName <$> lowerKind tvKind
lowerType (C.TCon tcName tcKind) = F.TCon tcName <$> lowerKind tcKind
lowerType (C.TApp t1 t2) = F.TApp <$> lowerType t1 <*> lowerType t2
lowerType (C.TSkol _ (C.MkTVar tvName tvKind)) = F.TVar tvName <$> lowerKind tvKind  -- error $ "lowerType: detected escaped skolem type constant: " <> show t
lowerType (C.TForall tvs ty) = do
    ty' <- lowerType ty
    foldrM (\(C.MkTVar tvName tvKind) r -> lowerKind tvKind <&> \k' -> F.TForall tvName k' r) ty' tvs
lowerType (C.TFun t1 t2) = F.TFun <$> lowerType t1 <*> lowerType t2
lowerType (C.TConstraint c ty) = F.TFun <$> lowerConstraint c <*> lowerType ty -- Constraints are desugard to dictionary applications

lowerConstraint :: C.Constraint -> Sem r F.Type
lowerConstraint (C.MkConstraint cname ty) = F.TApp (F.TCon cname (F.KFun F.KType F.KType)) <$> lowerType ty

lowerKind :: C.Kind -> Sem r F.Kind
lowerKind C.KStar = pure F.KType
lowerKind (C.KFun a b)= F.KFun <$> lowerKind a <*> lowerKind b
lowerKind C.KConstraint = pure F.KType -- Constraints are desugared to dictionary applications

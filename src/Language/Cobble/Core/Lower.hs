module Language.Cobble.Core.Lower where

import Language.Cobble.Prelude

import qualified Language.Cobble.Types as C

import qualified Language.Cobble.Core.Types as F
import Language.Cobble.Util.Bitraversable (secondM)

type CExpr = C.Expr C.Codegen
type CStatement = C.Statement C.Codegen
type CModule = C.Module C.Codegen

lower :: CModule -> Sem r [F.Decl]
lower (C.Module _deps mname sts) = lowerStmnts sts

lowerStmnts :: [CStatement] -> Sem r [F.Decl]
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
lowerStmnts (C.DefVariant{} : sts) = undefined

lowerExpr :: CExpr -> Sem r F.Expr
lowerExpr (C.App ty _ e1 e2) = F.App
                               <$> lowerExpr e1
                               <*> lowerExpr e2
lowerExpr (C.IntLit () _ n) = pure $ F.IntLit n
lowerExpr (C.UnitLit li) = pure $ F.UnitLit
lowerExpr (C.If ty _ c th el) = F.If 
                                <$> lowerExpr c
                                <*> lowerExpr th
                                <*> lowerExpr el
lowerExpr C.Let{} = undefined
-- lowerExpr (C.Let _ li (C.Decl (ty, _) f xs e1) e2) = F.Let f 
--                                 <$> lowerType ty
--                                 <*> lowerExpr (foldr (\(x, t) r -> C.Lambda t li x r) e1 xs) 
--                                 <*> lowerExpr e2
lowerExpr (C.Var _ _ x) = pure $ F.Var x
lowerExpr (C.Ascription _ _ e ty) = lowerExpr e -- Ascriptions are subsumed (hah) by type applications.
lowerExpr C.VariantConstr{} = undefined
lowerExpr C.Case{} = undefined
lowerExpr (C.Lambda (ty, argTy) _ x e) = F.Abs x <$> lowerType argTy <*> lowerExpr e
lowerExpr (C.ExprWrapper _ (C.WrapTyApp ty) e) = F.TyApp
                                                <$> lowerExpr e
                                                <*> lowerType ty
lowerExpr (C.ExprWrapper _ (C.WrapTyAbs (C.MkTVar tvName tvKind)) e) = F.TyAbs tvName <$> lowerKind tvKind <*> lowerExpr e
lowerExpr (C.ExprWrapper _ C.IdWrap e) = lowerExpr e

lowerType :: C.Type -> Sem r F.Type
lowerType (C.TVar (C.MkTVar tvName tvKind)) = F.TVar tvName <$> lowerKind tvKind
lowerType (C.TCon tcName tcKind) = F.TCon tcName <$> lowerKind tcKind
lowerType (C.TApp t1 t2) = F.TApp <$> lowerType t1 <*> lowerType t2
lowerType (C.TSkol (C.MkTVar tvName tvKind)) = F.TVar tvName <$> lowerKind tvKind -- ??? error $ "lowerType: detected skolem type constant. Does this mean it escaped?\n  Skolem: " <> show tv
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

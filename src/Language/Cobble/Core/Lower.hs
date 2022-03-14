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
lowerStmnts (C.Def _ _ (C.Decl _ x [] e) ty : sts) = (:)
    <$> do
        F.Def x
            <$> lowerType ty
            <*> lowerExpr e
    <*> lowerStmnts sts
lowerStmnts (s@C.Def{} : sts) = error "lowerStmnts: Definition with arguments persists after typechecking"
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
lowerExpr (C.Case t li e branches) = lowerCase t e branches
lowerExpr (C.Lambda (ty, argTy) _ x e) = F.Abs x <$> lowerType argTy <*> lowerExpr e
lowerExpr (C.TyApp _ ty e) = F.TyApp <$> lowerExpr e <*> lowerType ty
lowerExpr (C.TyAbs _ (C.MkTVar tvName tvKind) e) = F.TyAbs tvName <$> lowerKind tvKind <*> lowerExpr e

lowerVariantConstr :: Members '[Fresh Text QualifiedName] r => F.Type -> Int -> QualifiedName -> Seq F.Type -> Seq F.Expr -> Sem r F.Expr
lowerVariantConstr (F.TForall a k ty) i constrName tyArgs valArgs = do
    a' <- freshVar (C.originalName a)
    F.TyAbs a' k <$> lowerVariantConstr (F.replaceTVar a (F.TVar a' k) ty) i constrName (tyArgs |> F.TVar a' k) valArgs
lowerVariantConstr (F.TFun t1 t2) i constrName tyArgs valArgs = do
    x <- freshVar "x"
    F.Abs x t1 <$> lowerVariantConstr t2 i constrName tyArgs (valArgs |> F.Var x)
lowerVariantConstr ty i constrName tyArgs valArgs = do
    pure $ F.VariantConstr constrName i tyArgs valArgs

lowerCase :: forall r. Members '[Fresh Text QualifiedName] r 
          => C.Type 
          -> C.Expr C.Codegen 
          -> [C.CaseBranch C.Codegen] 
          -> Sem r F.Expr
lowerCase ty expr branches = do
    x <- freshVar "x"

    (jpDefs :: [F.Expr -> F.Expr], possibleBranches :: [([(QualifiedName, C.Pattern C.Codegen)], F.Expr)]) <- unzip <$> forM branches \(C.CaseBranch () _ p e) -> do
        jpName <- freshVar "j"
        e' <- lowerExpr e
        pure (F.Join jpName undefined undefined e'
            , ([(x, p)], F.Jump jpName undefined undefined undefined))


    body <- F.Let x <$> lowerType (C.getType expr) <*> lowerExpr expr <*> go [x] possibleBranches
    pure $ foldr ($) body jpDefs -- prepend join point definitions
    where
        go _ [] = error $ "Non-exhaustive patterns. (This should not be a panic)"
        go [] ((_, e):_) = pure e -- We always Pick the first instance 
        go (x:xs) possibleBranches = do
            let possiblePatterns = undefined
            
            caseBranches <- forM possiblePatterns \p -> do
                    possibleBranches' <- tryApplyPattern p possibleBranches
                    undefined
            undefined 

        tryApplyPattern :: QualifiedName -> F.Pattern -> ([(QualifiedName, C.Pattern C.Codegen)], F.Expr) -> Maybe ([(QualifiedName, C.Pattern C.Codegen)], F.Expr)
        tryApplyPattern x F.PWildcard (pats, e) = case filter (\(y, _) -> y == x) pats of
            [] -> Just (pats, e)
            _ -> Nothing
        tryApplyPattern x (F.PInt i) (pats, e) = (,e) . concat <$> traverse asIntPat pats
            where
                asIntPat :: (QualifiedName, C.Pattern C.Codegen) -> Maybe [(QualifiedName, C.Pattern C.Codegen)]
                asIntPat (y, p@(C.IntP _ j))
                    | x /= y = Just [(y, p)]
                    | i == j = Just []
                    | otherwise = Nothing
                asIntPat (y, p@(C.VarP _ x)) = Just [] -- TODO: Actually bind the variable
                asIntPat (y, p@C.ConstrP{}) = error $ "lowerCase: tried to lower a constructor pattern at an integer: " <> show p
        tryApplyPattern x (F.PConstr cname xs) (pats, e) = (,e) . concat <$> traverse asConstrPat pats
            where
                asConstrPat :: (QualifiedName, C.Pattern C.Codegen) -> Maybe [(QualifiedName, C.Pattern C.Codegen)]
                asConstrPat (y, p@C.IntP{}) = error $ "lowerCase: tried to lower an integer pattern at a variant constructor: " <> show p
                asConstrPat (y, p@(C.VarP _ x)) = Just [] -- TODO: Actually bind the variable
                asConstrPat (y, p@(C.ConstrP _ cname' subPats))
                    | x /= y || cname /= cname' = Just [(y, p)]
                    | otherwise = Just (zip xs subPats)

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

module Language.Cobble.Core.Lint where

import Language.Cobble.Prelude
import Language.Cobble.Types.QualifiedName
import Language.Cobble.Core.Types

data CoreLintError = VarNotFound QualifiedName LintEnv
                   | DeclTypeMismatch QualifiedName Type Type
                   | FunTypeMismatch Expr Expr Type Type
                   | LetTypeMismatch QualifiedName Expr Type Type
                   | IfCondTypeMismatch Expr Type Type
                   | IfBranchTypeMismatch Expr Expr Type Type
                   | AppOfNonFunction Expr Expr Type Type
                   | TyAppOfNonForall Expr Type Type
                   deriving (Show, Eq)

data LintEnv = LintEnv {
    varTypes :: Map QualifiedName Type
} deriving (Show, Eq)

insertType :: QualifiedName -> Type -> LintEnv -> LintEnv
insertType x t = LintEnv . insert x t . varTypes

lookupType :: Members '[Error CoreLintError] r 
           => QualifiedName 
           -> LintEnv 
           -> Sem r Type
lookupType x env = case lookup x (varTypes env) of
    Nothing -> throw (VarNotFound x env)
    Just ty -> pure ty

lint :: Members '[Error CoreLintError] r
     => LintEnv
     -> [Decl]
     -> Sem r ()
lint _ [] = pure ()
lint env (Def x ty e : ds) = do
    eTy <- lintExpr env e 
    when (eTy /= ty) $ throw (DeclTypeMismatch x ty eTy)
    lint (insertType x ty env) ds

lintExpr :: Members '[Error CoreLintError] r
         => LintEnv
         -> Expr
         -> Sem r Type
lintExpr env (Var x) = lookupType x env
lintExpr env (Abs x domTy body) = do
    bodyTy <- lintExpr (insertType x domTy env) body
    pure (TFun domTy bodyTy)
lintExpr env (App e1 e2) = do
    e1Ty <- lintExpr env e1
    e2Ty <- lintExpr env e2
    case e1Ty of
        TFun dom codom -> do
            typeMatch dom e2Ty (FunTypeMismatch e1 e2)
            pure codom
        _ -> throw $ AppOfNonFunction e1 e2 e1Ty e2Ty
lintExpr env (TyApp e ty) = do
    eTy <- lintExpr env e
    case eTy of
        TForall tv k eAppTy -> pure (replaceTVar tv ty eAppTy)
        _ -> throw $ TyAppOfNonForall e ty eTy
lintExpr env (TyAbs tv k e) = do
    eTy <- lintExpr env e
    pure (TForall tv k eTy)
lintExpr env (IntLit i) = pure intTy
lintExpr env UnitLit = pure unitTy
lintExpr env (Let x ty e1 e2) = do
    e1Ty <- lintExpr env e1
    typeMatch ty e1Ty (LetTypeMismatch x e1)
    lintExpr (insertType x ty env) e2
lintExpr env (If c th el) = do
    cTy <- lintExpr env c
    typeMatch cTy intTy (IfCondTypeMismatch c)
    thTy <- lintExpr env th
    elTy <- lintExpr env el
    typeMatch thTy elTy (IfBranchTypeMismatch th el)
    pure thTy

intTy :: Type
intTy = TCon (internalQName "Int") KType

boolTy :: Type
boolTy = TCon (internalQName "Bool") KType

unitTy :: Type
unitTy = TCon (internalQName "Unit") KType

typeMatch :: Members '[Error e] r 
          => Type 
          -> Type 
          -> (Type -> Type -> e) 
          -> Sem r ()
typeMatch t1 t2 mkErr
    | t1 == t2  = pure ()
    | otherwise = throw $ mkErr t1 t2

-- Does not handle kinds yet
replaceTVar :: QualifiedName -> Type -> Type -> Type
replaceTVar tv substTy ty@(TVar tv' _)
    | tv == tv' = substTy
    | otherwise = ty
replaceTVar tv substTy ty@(TForall tv' k ty')
    | tv == tv' = ty
    | otherwise = TForall tv' k (replaceTVar tv substTy ty')
replaceTVar _ _ ty@TCon{} = ty
replaceTVar tv substTy (TApp t1 t2) = TApp (replaceTVar tv substTy t1) (replaceTVar tv substTy t2)
replaceTVar tv substTy (TFun t1 t2) = TFun (replaceTVar tv substTy t1) (replaceTVar tv substTy t2)


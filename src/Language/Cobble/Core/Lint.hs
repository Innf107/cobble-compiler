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
                   | VariantArgMismatch QualifiedName Expr Type Type
                   | VariantMissingValArgs QualifiedName Type Type (Seq Type) (Seq Expr)
                   | VariantMissingTyArgs QualifiedName QualifiedName Kind Type (Seq Expr)
                   | VariantExcessiveArgs QualifiedName Type (Seq Type) (Seq Expr)
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
    let env' = insertType x ty env
    eTy <- lintExpr env' e 
    when (eTy /= ty) $ throw (DeclTypeMismatch x ty eTy)
    lint env' ds
lint env (DefVariant x args clauses : ds) = do
    let resKind = foldr (KFun . snd) KType args
    let resTy = foldl' (TApp) (TCon x resKind) (map (uncurry TVar) args)
    let constrTys = clauses <&> \(constr, constrArgs) ->
            (constr, (foldr (uncurry TForall) (foldr TFun resTy constrArgs) args))
    let env' = foldr (uncurry insertType) env constrTys
    lint env' ds

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
    typeMatch cTy boolTy (IfCondTypeMismatch c)
    thTy <- lintExpr env th
    elTy <- lintExpr env el
    typeMatch thTy elTy (IfBranchTypeMismatch th el)
    pure thTy
lintExpr env (VariantConstr x i tyArgs valArgs) = do
    xTy <- lookupType x env
    go xTy tyArgs valArgs
    where
        go (TFun t1 t2) tyArgs (arg :<| valArgs) = do
            argTy <- lintExpr env arg
            typeMatch t1 argTy (VariantArgMismatch x arg)
            go t2 tyArgs valArgs
        go (TFun t1 t2) tyArgs Empty = throw $ VariantMissingValArgs x t1 t2 tyArgs valArgs  

        go (TForall a _k ty) (tyArg :<| tyArgs) valArgs = do
            let ty' = replaceTVar a tyArg ty
            go ty' tyArgs valArgs
        go (TForall a k ty) Empty valArgs = throw $ VariantMissingTyArgs x a k ty valArgs
        go ty Empty Empty = pure ty
        go ty tyArgs valArgs = throw $ VariantExcessiveArgs x ty tyArgs valArgs
lintExpr env (Join _ _ _ _ _) = undefined
lintExpr env (Jump _ _ _ _) = undefined

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


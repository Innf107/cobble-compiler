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
                   | LintMsg Text
                   deriving (Show, Eq)

throwLint :: Members '[Error CoreLintError] r => Text -> Sem r a
throwLint = throw . LintMsg

data LintEnv = LintEnv {
    varTypes :: Map QualifiedName Type
-- `jpArgTypes` does *not* include the result type (which is always `∀r. r`).
-- This means, if (j : (a*, σ*)) ∈ jpArgTypes, the actual type of j is `∀a*. σ* -> ∀r. r)`.
,   jpArgTypes :: Map QualifiedName (Seq (QualifiedName, Kind), Seq Type)
} deriving (Show, Eq)

insertType :: QualifiedName -> Type -> LintEnv -> LintEnv
insertType x t LintEnv{varTypes, jpArgTypes}= 
    LintEnv {
            varTypes = insert x t varTypes
        ,   jpArgTypes
        }

lookupType :: Members '[Error CoreLintError] r 
           => QualifiedName 
           -> LintEnv 
           -> Sem r Type
lookupType x env = case lookup x (varTypes env) of
    Nothing -> throw (VarNotFound x env)
    Just ty -> pure ty

clearJPs :: LintEnv -> LintEnv
clearJPs env = env {jpArgTypes = mempty}

lookupJP :: Members '[Error CoreLintError] r
         => QualifiedName
         -> LintEnv
         -> Sem r (Seq (QualifiedName, Kind), Seq Type)
lookupJP j env@LintEnv{jpArgTypes} = case lookup j jpArgTypes of
    Just pars -> pure pars
    Nothing -> throwLint $ "Join point '" <> show j <> "'not found in the local context. Did a jump expression escape?\nContext: " <> show env

lint :: Members '[Error CoreLintError] r
     => LintEnv
     -> [Decl]
     -> Sem r ()
lint _ [] = pure ()
lint env (Def x ty e : ds) = do
    let env' = insertType x ty $ clearJPs env
    eTy <- lintExpr env' e 
    when (eTy /= ty) $ throw (DeclTypeMismatch x ty eTy)
    lint env' ds
lint env (DefVariant x args clauses : ds) = do
    let resKind = foldr (KFun . snd) KType args
    let resTy = foldl' (TApp) (TCon x resKind) (map (uncurry TVar) args)
    let constrTys = clauses <&> \(constr, constrArgs) ->
            (constr, (foldr (uncurry TForall) (foldr TFun resTy constrArgs) args))
    let env' = clearJPs $ foldr (uncurry insertType) env constrTys
    lint env' ds

lintExpr :: Members '[Error CoreLintError] r
         => LintEnv
         -> Expr
         -> Sem r Type
lintExpr env (Var x) = lookupType x env
lintExpr env (Abs x domTy body) = do
    bodyTy <- lintExpr (insertType x domTy $ clearJPs env) body
    pure (TFun domTy bodyTy)
-- See note [clearJPs for App]
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
    eTy <- lintExpr (clearJPs env) e
    pure (TForall tv k eTy)
lintExpr env (IntLit i) = pure intTy
lintExpr env UnitLit = pure unitTy
lintExpr env (Let x ty e1 e2) = do
    e1Ty <- lintExpr env e1 -- See note [clearJPs for App].
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
-- TODO: Not quite sure how to check case exprssions and join points yet
lintExpr env (Case e branches) = do
    undefined
lintExpr env (Join _ _ _ _ _) = undefined
lintExpr env (Jump j tyArgs valArgs retTy) = do
    (tyPars, valPars) <- lookupJP j env
    when (length tyPars /= length tyArgs) 
        $ throwLint $ "Mismatched type argument count for jump point '" <> show j <> "'.\n    Expected: " <> show tyPars <> "\n    Actual: " <> show tyArgs
    zipWithM_ 
        (\(_, k) ty -> when (k /= getKind ty) $ throwLint $ "Mismatched kinds for jump point arguments.\n    Expected: " <> show k <> ".\n    Actual: (" <> show ty <> ":" <> show (getKind ty) <> ").") 
        (toList tyPars) 
        (toList tyArgs)
    when (length tyPars /= length tyArgs) 
        $ throwLint $ "Mismatched type argument count for jump point '" <> show j <> "'.\nExpected: " <> show tyPars <> "\nActual: " <> show tyArgs
    let appliedTypes = foldr (\((a, _), ty) rs -> fmap (replaceTVar a ty) rs) valPars $ zip tyPars tyArgs 
    zipWithM_ (\expected expr -> lintExpr (clearJPs env) expr >>= \exprTy -> typeMatch expected exprTy \t1 t2 -> LintMsg $ "Argument mismatch for join point '" <> show j <> "'.\n    Expected: " <> show expected <> ".\n    Actual: " <> show expr <> ":" <> show exprTy) 
        (toList appliedTypes) 
        (toList valArgs)
    pure retTy

{- note [clearJPs for App]
In 'Compiling without continuations'[1], the rule for applications clearly states
that join point contexts should be cleared for function arguments, but not for the evaluation
of the actual function.
However, since Cobble and, by extension, Core are Call by Value, we should be fine with allowing arguments to functions to 
keep the join point context, since we can be sure that they will be evaluated immediately and, crucially, on the same stack.

The same reasoning applies to let expressions, which behave more like single-branch case expressions in lazy System F.

[1] TODO: Link
-}

getKind :: Type -> Kind
getKind = undefined

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


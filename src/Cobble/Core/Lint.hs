module Cobble.Core.Lint where

import Cobble.Prelude
import Cobble.Types.QualifiedName
import Cobble.Core.Types

newtype CoreLintError = MkCoreLintError Text deriving (Show, Eq)

throwLint :: Members '[Error CoreLintError] r => Text -> Sem r a
throwLint = throw . MkCoreLintError

data LintEnv = LintEnv {
    varTypes :: Map QualifiedName Type
-- `jpArgTypes` does *not* include the result type (which is always `∀r. r`).
-- This means, if (j : (a*, σ*)) ∈ jpArgTypes, the actual type of j is `∀a*. σ* -> ∀r. r)`.
,   jpArgTypes :: Map QualifiedName (Seq (QualifiedName, Kind), Seq Type)
,   dictTyDefs :: Map QualifiedName (Seq (QualifiedName, Kind), Seq (QualifiedName, Type))
} deriving (Show, Eq)

insertType :: QualifiedName -> Type -> LintEnv -> LintEnv
insertType x t env@LintEnv{varTypes}= 
    env {
            varTypes = insert x t varTypes
        }

lookupType :: Members '[Error CoreLintError] r 
           => QualifiedName 
           -> LintEnv 
           -> Sem r Type
lookupType x env = case lookup x (varTypes env) of
    Nothing -> throwLint $ "Variable not found: " <> show x <> "\nContext: " <> show env
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

insertJP :: QualifiedName
         -> Seq (QualifiedName, Kind)
         -> Seq Type
         -> LintEnv
         -> LintEnv
insertJP j tyParams valParams env@LintEnv{jpArgTypes} = 
    env{
        jpArgTypes = insert j (tyParams, valParams) jpArgTypes
    }

lookupDictTy :: Members '[Error CoreLintError] r
             => QualifiedName
             -> LintEnv
             -> Sem r (Seq (QualifiedName, Kind), Seq (QualifiedName, Type))
lookupDictTy dictName env@LintEnv{dictTyDefs} = case lookup dictName dictTyDefs of
    Just def -> pure def
    Nothing -> throwLint $ "Dictionary type '" <> show dictName <> "' not found.\nContext: " <> show env

insertDictTy :: QualifiedName
             -> Seq (QualifiedName, Kind)
             -> Seq (QualifiedName, Type)
             -> LintEnv
             -> LintEnv
insertDictTy dictName tvs fields env@LintEnv{dictTyDefs} =
    env {
        dictTyDefs = insert dictName (tvs, fields) dictTyDefs
    }

lint :: Members '[Error CoreLintError] r
     => LintEnv
     -> Seq Decl
     -> Sem r ()
lint _ Empty = pure ()
lint env (Def x ty e :<| ds) = do
    let env' = insertType x ty $ clearJPs env
    eTy <- lintExpr env' e 
    typeMatch eTy ty $ "Type mismatch in declaration for '" <> show x <> "'"
    lint env' ds
lint env (DefVariant x args clauses :<| ds) = do
    let resKind = foldr (KFun . snd) KType args
    let resTy = foldl' (TApp) (TCon x resKind) (map (uncurry TVar) args)
    let constrTys = clauses <&> \(constr, constrArgs) ->
            -- TODO: Include effect variables in function types
            undefined -- (constr, (foldr (uncurry TForall) (foldr TFun resTy constrArgs) args))
    let env' = clearJPs $ foldr (uncurry insertType) env constrTys
    lint env' ds
lint env (DefDict x args fields :<| ds) = do
    let env' = insertDictTy x args (fromList $ toList fields) env
    lint env' ds

lintExpr :: forall r. Members '[Error CoreLintError] r
         => LintEnv
         -> Expr
         -> Sem r Type
lintExpr env (Var x) = lookupType x env
lintExpr env (Abs x eff domTy body) = do
    bodyTy <- lintExpr (insertType x domTy $ clearJPs env) body
    pure (TFun domTy eff bodyTy)
-- See note [clearJPs for App]
lintExpr env (App e1 e2) = do
    e1Ty <- lintExpr env e1
    e2Ty <- lintExpr env e2
    case e1Ty of
        TFun dom eff codom -> do
            -- TODO: handle eff?
            typeMatch dom e2Ty $ "Function type mismatch.\n    Function: " <> show e1 <> "\n    Argument: " <> show e2
            pure codom
        _ -> throwLint $ "Application of value with non-function type '" <> show e1Ty <> "'\n    'Function' expression: " <> show e1 <> "\n    Argument type: " <> show e2Ty <> "\n    Argument expression: " <> show e2
lintExpr env (TyApp e ty) = do
    eTy <- lintExpr env e
    case eTy of
        TForall tv k eAppTy -> pure (replaceTVar tv ty eAppTy)
        _ -> throwLint $ "Type application at non-forall type '" <> show eTy <> "'\n    Expression: " <> show e <> "\n    Applied type: " <> show ty
lintExpr env (TyAbs tv k e) = do
    eTy <- lintExpr (clearJPs env) e
    pure (TForall tv k eTy)
lintExpr env (IntLit i) = pure intTy
lintExpr env (Let x ty e1 e2) = do
    e1Ty <- lintExpr env e1 -- See note [clearJPs for App].
    typeMatch ty e1Ty $ "let type mismatch in binding of '" <> show x <> "' to expression: " <> show e1
    lintExpr (insertType x ty env) e2
lintExpr env (If c th el) = do
    cTy <- lintExpr env c
    typeMatch cTy boolTy $ "Condition of if expression is not a boolean: " <> show c
    thTy <- lintExpr env th
    elTy <- lintExpr env el
    typeMatch thTy elTy $ "Branches of if expression have different types.\n    Branch 1: " <> show th <> "\n    Branch 2: " <> show el
    pure thTy
lintExpr env (VariantConstr x i tyArgs valArgs) = do
    xTy <- lookupType x env
    lintSaturated env (show x) xTy tyArgs valArgs
lintExpr env (Case scrut branches) = do
    scrutTy <- lookupType scrut env
    resTys <- forM branches \(pat, e) -> do
        matchPatTy env pat scrutTy
        lintExpr (bindPatVars pat env) e
    case resTys of
        Empty -> throwLint $ "Empty case on scrutinee '" <> show scrut <> "'. This should probably be supported at some point."
        (ty :<| tys) -> 
            foldrM 
                (\branchResTy fullResTy -> typeMatch branchResTy fullResTy ("Mismatched case branches in case for scrutinee '" <> show scrut <> "'") >> pure fullResTy)
                ty 
                tys
    where
        matchPatTy :: LintEnv -> Pattern -> Type -> Sem r ()
        matchPatTy env pat@PInt{} ty            = typeMatch ty intTy $ "Mismatched scrutinee and pattern types in case on '" <> show scrut <> "' with pattern: " <> show pat
        matchPatTy env PWildcard{} _            = pure ()
        matchPatTy env pat@(PConstr cname _ _) ty   = do
            -- There are no GADTs (yet), so all constructor types are fully polymorphic
            -- and we can cheat a little, by just looking at the head type constructor, since everything else is
            -- fully polymorphic.
            constrTy <- headTyCon <$> lookupType cname env
            
            typeMatch (headTyCon ty) constrTy $ "Mismatched scrutinee and pattern types in case on '" <> show scrut <> "' with pattern: " <> show pat
        
        bindPatVars :: Pattern -> LintEnv -> LintEnv
        bindPatVars PInt{} env              = env
        bindPatVars PWildcard env           = env
        bindPatVars (PConstr _ vars _) env  = foldr (uncurry insertType) env vars

-- TODO: No kind checks yet
lintExpr env (Join j tyParams valParams body e) = do
    let innerEnv = foldr (uncurry insertType) env valParams
    bodyTy <- lintExpr innerEnv body

    let remainingEnv = insertJP j tyParams (fmap snd valParams) env
    eTy <- lintExpr remainingEnv e
    
    typeMatch bodyTy eTy $ "Jump point body type and return type do not match for join point '" <> show j <> "'"
    pure eTy

lintExpr env (Jump j tyArgs valArgs retTy) = do
    (tyPars, valPars) <- lookupJP j env
    when (length tyPars /= length tyArgs) 
        $ throwLint $ "Mismatched type argument count for jump point '" <> show j <> "'.\n    Expected: " <> show tyPars <> "\n    Actual: " <> show tyArgs
    zipWithM_ 
        (\(_, k) ty -> getKind ty >>= \tyK -> when (k /= tyK) $ throwLint $ "Mismatched kinds for jump point arguments.\n    Expected: " <> show k <> ".\n    Actual: (" <> show ty <> ":" <> show tyK <> ").") 
        tyPars
        tyArgs
    when (length valPars /= length valArgs) 
        $ throwLint $ "Mismatched value argument count for jump point '" <> show j <> "'.\nExpected: " <> show valPars <> "\nActual: " <> show valArgs
    let appliedTypes = foldr (\((a, _), ty) rs -> fmap (replaceTVar a ty) rs) valPars $ zip tyPars tyArgs 
    zipWithM_ (\expected expr -> lintExpr (clearJPs env) expr >>= \exprTy -> typeMatch expected exprTy $ "Argument mismatch for join point '" <> show j <> "' with body '" <> show expr <> "'")
        appliedTypes
        valArgs
    pure retTy

lintExpr env (PrimOp op ty tyArgs valArgs) = lintSaturated env (show op) ty tyArgs valArgs

lintExpr env (DictConstruct className tyArgs fields) = do
    (tyParams, dictFieldTys) <- lookupDictTy className env
    when (length tyParams /= length tyArgs)
        $ throwLint $ "Mismatched type argument count at dict construction for '" <> show className <> "'.\nExpected: " <> show tyParams <> "\nActual: " <> show tyArgs
    zipWithM_ 
        (\(_, k) ty -> getKind ty >>= \tyK -> when (k /= tyK) $ throwLint $ "Mismatched kinds for dict access arguments for dict '" <> show className <> "'.\n    Expected: " <> show k <> ".\n    Actual: (" <> show ty <> ":" <> show tyK <> ").")
        tyParams
        tyArgs
    zipWithM_
        (\expr (_, ty) -> do
            exprTy <- lintExpr env expr
            appliedTy <- applyTypes ty tyArgs 
            typeMatch exprTy appliedTy $ "Mismatched dictionary construction argument for class '" <> show className <> "'."
            )
        fields
        dictFieldTys
    let dictKind = foldr (KFun . snd) KType tyParams
    pure $ foldl' TApp (TCon className dictKind) tyArgs

lintExpr env (DictAccess e className tyArgs field) = do
    (tyParams, fieldTys) <- lookupDictTy className env
    when (length tyParams /= length tyArgs)
        $ throwLint $ "Mismatched type argument count at dict access for '" <> show className <> "', computed by '" <> show e <> "'.\nExpected: " <> show tyParams <> "\nActual: " <> show tyArgs
    zipWithM_ 
        (\(_, k) ty -> getKind ty >>= \tyK -> when (k /= tyK) $ throwLint $ "Mismatched kinds for dict access arguments for dict '" <> show className <> ", computed by " <> show e <> "'.\n    Expected: " <> show k <> ".\n    Actual: (" <> show ty <> ":" <> show tyK <> ").")
        tyParams
        tyArgs
    case snd <$> find ((== field) . fst) fieldTys of
        Nothing -> throwLint $ "Nonexistant dictionary field '" <> show field <> "'. In dictionary '" <> show className <> "', computed by '" <> show e <> "'."
        Just ty -> applyTypes ty tyArgs

lintSaturated :: Members '[Error CoreLintError] r => LintEnv -> Text -> Type -> Seq Type -> Seq Expr -> Sem r Type
lintSaturated env x tyArgs valArgs = go tyArgs valArgs
    where
        go (TFun t1 eff t2) tyArgs (arg :<| valArgs) = do
            -- TODO: Handle eff
            argTy <- lintExpr env arg
            typeMatch t1 argTy $ "Argument type mismatch for variant constructor or primop '" <> x <> "' with argument: " <> show arg
            go t2 tyArgs valArgs
        go (TFun t1 eff t2) tyArgs Empty = throwLint $ "Variant constructor or primop '" <> x <> "' is missing value arguments.\n    Remaining type: " <> show (TFun t1 eff t2) <> "\n    Applied value arguments: " <> show valArgs <> "\n    Not yet applied type arguments: " <> show tyArgs  

        go (TForall a _k ty) (tyArg :<| tyArgs) valArgs = do
            let ty' = replaceTVar a tyArg ty
            go ty' tyArgs valArgs
        go (TForall a k ty) Empty valArgs = throwLint $ "Variant constructor or primop '" <> x <> "' is missing type arguments.\n    Remaining type: " <> show (TForall a k ty) <> "\n    Not yet applied value arguments: " <> show valArgs <> "\n    Applied type arguments: " <> show tyArgs
        go ty Empty Empty = pure ty
        go ty tyArgs valArgs = throwLint $ "Excessive arguments for variant constructor or primop '" <> x <> "'.\n    Remaining type: " <> show ty <> "\n    Not yet applied type arguments: " <> show tyArgs <> "\n    Mpt yet applied value arguments: " <> show valArgs

{- note [clearJPs for App]
In 'Compiling without continuations'[1], the rule for applications clearly states
that join point contexts should be cleared for function arguments, but not for the evaluation
of the actual function.
However, since Cobble and, by extension, Core are Call by Value, we should be fine if we allow arguments to functions to 
keep the join point context, since we can be sure that they will be evaluated immediately and, crucially, on the same stack.

The same reasoning applies to let expressions, which behave more like single-branch case expressions in lazy System F.

[1]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/join-points-pldi17.pdf
-}

applyTypes :: Members '[Error CoreLintError] r => Type -> Seq Type -> Sem r Type
applyTypes ty args = foldrM applyType ty args

applyType :: Members '[Error CoreLintError] r => Type -> Type -> Sem r Type
applyType ty (TForall a _ ty') = pure $ replaceTVar a ty ty'
applyType ty ty' = throwLint $ "Excessive application with argument '" <> show ty <> "' in application of type '" <> show ty' <> "'"

headTyCon :: Type -> Type
headTyCon (TForall _ _ ty) = headTyCon ty
headTyCon (TFun a eff b) = headTyCon b
headTyCon (TApp a b) = headTyCon a 
headTyCon ty = ty

getKind :: Members '[Error CoreLintError] r => Type -> Sem r Kind
getKind (TFun a eff b)   = pure KType
getKind (TVar _ k)       = pure k
getKind (TCon _ k)       = pure k
getKind (TForall _ _ ty) = getKind ty
getKind (TApp a b) = do
    bKind <- getKind b
    getKind a >>= \case
        KFun dom cod 
            | dom == bKind -> pure cod
        aKind -> throwLint $ "Cannot apply type (" <> show a <> " : " <> show aKind <> ") to argument type (" <> show b <> " : " <> show bKind <> ")"
getKind TRowNil = pure $ KRow undefined -- What kind should fully polymorphic rows have?
getKind (TRowExtend tys row) = getKind row

typeMatch :: Members '[Error CoreLintError] r 
          => Type 
          -> Type 
          -> Text
          -> Sem r ()
typeMatch t1 t2 msg
    | t1 == t2  = pure ()
    | otherwise = throw $ MkCoreLintError $ msg 
                                        <> "\n    Expected: " <> show t1
                                        <> "\n      Actual: " <> show t2


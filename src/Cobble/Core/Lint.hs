module Cobble.Core.Lint where

import Cobble.Prelude
import Cobble.Syntax.QualifiedName
import Cobble.Core.Syntax

import Cobble.Util

newtype CoreLintError = MkCoreLintError Text deriving (Show, Eq)

throwLint :: Members '[Error CoreLintError] r => Text -> Sem r a
throwLint = throw . MkCoreLintError

data LintEnv = LintEnv {
    varTypes :: Map QualifiedName Type
-- `jpArgTypes` does *not* include the result type (which is always `∀r. r`).
-- This means, if (j : (a*, σ*)) ∈ jpArgTypes, the actual type of j is `∀a*. σ* -> ∀r. r)`.
,   jpArgTypes :: Map QualifiedName (Seq (QualifiedName, Kind), Seq Type)
,   dictTyDefs :: Map QualifiedName (Seq (QualifiedName, Kind), Seq (QualifiedName, Type))
,   effDefs    :: Map QualifiedName (Seq (QualifiedName, Kind), Seq (QualifiedName, Type))
,   opEffs     :: Map QualifiedName QualifiedName
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

insertEff :: QualifiedName
          -> Seq (QualifiedName, Kind)
          -> Seq (QualifiedName, Type)
          -> LintEnv
          -> LintEnv
insertEff effName tvs ops env@LintEnv{effDefs, opEffs} =
    env {
        effDefs = insert effName (tvs, ops) effDefs
    ,   opEffs = foldr (\(op, _) r -> insert op effName r) opEffs ops
    }

lookupOp :: Members '[Error CoreLintError] r
          => QualifiedName
          -> LintEnv
          -> Sem r QualifiedName
lookupOp opName env@LintEnv{opEffs} = case lookup opName opEffs of
    Just effName -> pure effName
    Nothing -> throwLint $ "Effect operation '" <> show opName <> "' not found.\nContext: " <> show env

lookupEff :: Members '[Error CoreLintError] r
          => QualifiedName
          -> LintEnv
          -> Sem r (Seq (QualifiedName, Kind), Seq (QualifiedName, Type))
lookupEff effName env@LintEnv{effDefs} = case lookup effName effDefs of
    Just def -> pure def
    Nothing -> throwLint $ "Effect '" <> show effName <> "' not found.\nContext: " <> show env

lint :: Members '[Error CoreLintError] r
     => LintEnv
     -> Seq Decl
     -> Sem r ()
lint _ Empty = pure ()
lint env (Def x ty e :<| ds) = do
    let env' = insertType x ty $ clearJPs env
    eTy <- lintExpr env' e TRowNil
    typeMatch eTy ty $ "Type mismatch in declaration for '" <> show x <> "'"
    lint env' ds
lint env (DefVariant x args clauses :<| ds) = do
    let resKind = foldr (KFun . snd) KType args
    let resTy = foldl' (TApp) (TCon x resKind) (map (uncurry TVar) args)
    let constrTys = clauses <&> \(constr, constrArgs) ->
            (constr, (foldr (uncurry TForall) (foldr (\x r -> TFun x TEffUR r) resTy constrArgs) args))
    let env' = clearJPs $ foldr (uncurry insertType) env constrTys
    lint env' ds
lint env (DefDict x args fields :<| ds) = do
    let env' = insertDictTy x args (fromList $ toList fields) env
    lint env' ds
lint env (DefEffect x args fields :<| ds) = do
    let env' = insertEff x args fields env
    lint env' ds

lintExpr :: forall r. Members '[Error CoreLintError] r
         => LintEnv
         -> Expr
         -> Effect
         -> Sem r Type
lintExpr env (Var x) eff = lookupType x env
lintExpr env (Abs x lamEff domTy body) _eff = do
    bodyTy <- lintExpr (insertType x domTy $ clearJPs env) body lamEff
    pure (TFun domTy lamEff bodyTy)
-- See note [clearJPs for App]
lintExpr env (App e1 e2) eff = do
    e1Ty <- lintExpr env e1 eff
    e2Ty <- lintExpr env e2 eff
    case e1Ty of
        TFun dom funEff codom -> do
            typeMatch dom e2Ty $ "Function type mismatch.\n    Function: " <> show e1 <> "\n    Argument: " <> show e2
            typeMatch funEff eff $ "Effect mismatch in application.\n    Function: " <> show e1 <> "\n    Argument: " <> show e2
            pure codom
        _ -> throwLint $ "Application of value with non-function type '" <> show e1Ty <> "'\n    'Function' expression: " <> show e1 <> "\n    Argument type: " <> show e2Ty <> "\n    Argument expression: " <> show e2
lintExpr env (TyApp e ty) eff = do
    eTy <- lintExpr env e eff
    case eTy of
        TForall tv k eAppTy -> pure (replaceTVar tv ty eAppTy)
        _ -> throwLint $ "Type application at non-forall type '" <> show eTy <> "'\n    Expression: " <> show e <> "\n    Applied type: " <> show ty
lintExpr env (TyAbs tv k e) eff = do
    eTy <- lintExpr (clearJPs env) e eff
    pure (TForall tv k eTy)
lintExpr env (IntLit i) _eff = pure intTy
lintExpr env (Let x ty e1 e2) eff = do
    e1Ty <- lintExpr env e1 eff -- See note [clearJPs for App].
    typeMatch ty e1Ty $ "let type mismatch in binding of '" <> show x <> "' to expression: " <> show e1
    lintExpr (insertType x ty env) e2 eff
lintExpr env (If c th el) eff = do
    cTy <- lintExpr env c eff
    typeMatch cTy boolTy $ "Condition of if expression is not a boolean: " <> show c
    thTy <- lintExpr env th eff
    elTy <- lintExpr env el eff
    typeMatch thTy elTy $ "Branches of if expression have different types.\n    Branch 1: " <> show th <> "\n    Branch 2: " <> show el
    pure thTy
lintExpr env (VariantConstr x i tyArgs valArgs) eff = do
    xTy <- lookupType x env
    lintSaturated env (show x) "variant constructor" xTy tyArgs valArgs eff
lintExpr env (Case scrut branches) eff = do
    scrutTy <- lookupType scrut env
    resTys <- forM branches \(pat, e) -> do
        matchPatTy env pat scrutTy
        lintExpr (bindPatVars pat env) e eff
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
lintExpr env (Join j tyParams valParams body e) eff = do
    let innerEnv = foldr (uncurry insertType) env valParams
    bodyTy <- lintExpr innerEnv body eff

    let remainingEnv = insertJP j tyParams (fmap snd valParams) env
    eTy <- lintExpr remainingEnv e eff
    
    typeMatch bodyTy eTy $ "Jump point body type and return type do not match for join point '" <> show j <> "'"
    pure eTy

lintExpr env (Jump j tyArgs valArgs retTy) eff = do
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
    zipWithM_ (\expected expr -> lintExpr (clearJPs env) expr eff >>= \exprTy -> typeMatch expected exprTy $ "Argument mismatch for join point '" <> show j <> "' with body '" <> show expr <> "'")
        appliedTypes
        valArgs
    pure retTy

lintExpr env (DictConstruct className tyArgs fields) eff = do
    (tyParams, dictFieldTys) <- lookupDictTy className env
    when (length tyParams /= length tyArgs)
        $ throwLint $ "Mismatched type argument count at dict construction for '" <> show className <> "'.\nExpected: " <> show tyParams <> "\nActual: " <> show tyArgs
    zipWithM_ 
        (\(_, k) ty -> getKind ty >>= \tyK -> when (k /= tyK) $ throwLint $ "Mismatched kinds for dict access arguments for dict '" <> show className <> "'.\n    Expected: " <> show k <> ".\n    Actual: (" <> show ty <> ":" <> show tyK <> ").")
        tyParams
        tyArgs
        -- TODO: Couldn't this just use lintSaturated?
    zipWithM_
        (\expr (_, ty) -> do
            exprTy <- lintExpr env expr eff
            appliedTy <- applyTypes (foldr (uncurry TForall) ty tyParams) tyArgs
            typeMatch exprTy appliedTy $ "Mismatched dictionary construction argument for class '" <> show className <> "'."
            )
        fields
        dictFieldTys
    let dictKind = foldr (KFun . snd) KType tyParams
    pure $ foldl' TApp (TCon className dictKind) tyArgs

lintExpr env (DictAccess e className tyArgs field) eff = do
    eTy <- lintExpr env e eff
    
    -- TODO: ??? typeMatch eTy (foldl' TApp (TCon className undefined) (map (uncurry TVar) tyArgs)) $ "Type mismatch in dict access: "

    -- TODO: Add foralls of tyArgs to fieldTy before matching

    (tyParams, fieldTys) <- lookupDictTy className env
    when (length tyParams /= length tyArgs)
        $ throwLint $ "Mismatched type argument count at dict access for '" <> show className <> "', computed by '" <> show e <> "'.\nExpected: " <> show tyParams <> "\nActual: " <> show tyArgs
    zipWithM_ 
        (\(_, k) ty -> getKind ty >>= \tyK -> when (k /= tyK) $ throwLint $ "Mismatched kinds for dict access arguments for dict '" <> show className <> ", computed by " <> show e <> "'.\n    Expected: " <> show k <> ".\n    Actual: (" <> show ty <> ":" <> show tyK <> ").")
        tyParams
        tyArgs
    case snd <$> find ((== field) . fst) fieldTys of
        Nothing -> throwLint $ "Nonexistant dictionary field '" <> show field <> "'. In dictionary '" <> show className <> "', computed by '" <> show e <> "'."
        Just ty -> applyTypes (foldr (uncurry TForall) ty tyParams) tyArgs

lintExpr env (Perform opEff op tyArgs valArgs) eff = do
    (tvars, opTys) <- lookupEff opEff env
    let opTy = fromMaybe (error "op not in opTys, this really shouldn't happen, what the hell did you do?!") 
            $ lookupSeq op opTys
    checkRowContainsEff opEff eff $ "Operation '" <> show op <> "' requires the effect '" <> show opEff <> "' which was not found in the current effect context '" <> show eff <> "'"
    lintSaturated env (show op) "operation" opTy tyArgs valArgs eff
lintExpr env Handle{} eff = undefined

lintSaturated :: Members '[Error CoreLintError] r => LintEnv -> Text -> Text -> Type -> Seq Type -> Seq Expr -> Effect -> Sem r Type
lintSaturated env x source ty tyArgs valArgs eff = go ty tyArgs valArgs
    where
        go (TFun t1 eff t2) tyArgs (arg :<| valArgs) = do
            argTy <- lintExpr env arg eff
            typeMatch t1 argTy $ "Argument type mismatch for " <> source <> " '" <> x <> "' with argument: " <> show arg
            go t2 tyArgs valArgs
        go (TFun t1 eff t2) tyArgs Empty = throwLint $ source <>  " '" <> x <> "' is missing value arguments.\n    Remaining type: " <> show (TFun t1 eff t2) <> "\n    Applied value arguments: " <> show valArgs <> "\n    Not yet applied type arguments: " <> show tyArgs  

        go (TForall a _k ty) (tyArg :<| tyArgs) valArgs = do
            let ty' = replaceTVar a tyArg ty
            go ty' tyArgs valArgs
        go (TForall a k ty) Empty valArgs = throwLint $ source <> " '" <> x <> "' is missing type arguments.\n    Remaining type: " <> show (TForall a k ty) <> "\n    Not yet applied value arguments: " <> show valArgs <> "\n    Applied type arguments: " <> show tyArgs
        go ty Empty Empty = pure ty
        go ty tyArgs valArgs = throwLint $ "Excessive arguments for " <> source <> " '" <> x <> "'.\n    Remaining type: " <> show ty <> "\n    Not yet applied type arguments: " <> show tyArgs <> "\n    Mpt yet applied value arguments: " <> show valArgs

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
getKind TFun{}           = pure KType
getKind (TVar _ k)       = pure k
getKind (TCon _ k)       = pure k
getKind (TForall _ _ ty) = getKind ty
getKind (TApp a b) = do
    bKind <- getKind b
    getKind a >>= \case
        KFun dom cod 
            | dom == bKind -> pure cod
        aKind -> throwLint $ "Cannot apply type (" <> show a <> " : " <> show aKind <> ") to argument type (" <> show b <> " : " <> show bKind <> ")"
getKind TRowNil = pure $ KRow KEffect -- TODO: We hardcode this kind to effect right now. This is going to be an issue once we have extensible records.
getKind (TRowExtend tys row) = getKind row
getKind TEffUR = pure $ KRow KEffect

typeMatch :: Members '[Error CoreLintError] r 
          => Type
          -> Type
          -> Text
          -> Sem r ()
typeMatch t1 t2 msg = case typeEquiv t1 t2 of
    Left err -> throw $ MkCoreLintError $ msg 
                                        <> "\n    Expected: " <> show t1
                                        <> "\n      Actual: " <> show t2
                                        <> "\nSpecifically: " <> err
    Right () -> pure ()
typeEquiv :: Type -> Type -> Either Text ()
typeEquiv (TVar v1 k1) (TVar v2 k2)
    | v1 == v2 && k1 == k2 = pure ()
typeEquiv (TCon con1 k1) (TCon con2 k2)
    | con1 == con2 && k1 == k2 = pure ()
typeEquiv (TFun dom1 eff1 cod1) (TFun dom2 eff2 cod2) = do
    typeEquiv dom1 dom2
    typeEquiv eff1 eff2
    typeEquiv cod1 cod2
typeEquiv (TApp tfun1 targ1) (TApp tfun2 targ2) = do
    typeEquiv tfun1 tfun2
    typeEquiv targ1 targ2
typeEquiv (TForall a1 k1 ty1) (TForall a2 k2 ty2)
    | k1 == k2 = typeEquiv ty1 (replaceTVar a2 (TVar a1 k1) ty2)
typeEquiv TRowNil TRowNil = pure ()
-- Empty extensions should be irrelevant
typeEquiv (TRowExtend Empty ty1) ty2 = typeEquiv ty1 ty2
typeEquiv ty1 (TRowExtend Empty ty2) = typeEquiv ty1 ty2
typeEquiv t1@TRowExtend{} t2@TRowExtend{} = do
    let (tys1, end1) = collectRowExtensions t1
    let (tys2, end2) = collectRowExtensions t2
    typeEquiv end1 end2
    go tys1 tys2
        where
            collectRowExtensions (TRowExtend tys row) = first (tys<>) $ collectRowExtensions row
            collectRowExtensions ty = ([], ty)
            go Empty Empty = pure ()
            go (field1 :<| remaining1) t2s = 
                -- We look up types by (==), not typeEquiv, since effect constructors should
                -- be comparable by identity anyway.
                case lookupAndDeleteWith (guard . (== field1)) t2s of
                    Just ((), remaining2) -> go remaining1 remaining2
                    Nothing -> Left $ "Row is missing field '" <> show field1
                                <> "'\n       row fields: " <> show t2s
                                <> "'\n    when matching '" <> show t1
                                <> "'\n             with '" <> show t2 <> "'"
            go remaining1 remaining2 = Left $ "Unable to match row extensions '" <> show remaining1 <> "' and '" <> show remaining2 
                                             <> "'\n    Trying to match row type '" <> show t1 
                                             <> "'\n                        with '" <> show t2 <> "'"
typeEquiv TEffUR _ = pure ()
typeEquiv _ TEffUR = pure ()
typeEquiv t1 t2 = Left $ "Unable to match: \n"
                      <> "    Expected: " <> show t1 <> "\n    " 
                      <> "      Actual: " <> show t2 

checkRowContainsEff :: Members '[Error CoreLintError] r
                    => QualifiedName 
                    -> Effect 
                    -> Text 
                    -> Sem r ()
checkRowContainsEff _ TEffUR _ = pure ()
checkRowContainsEff _ TRowNil msg = throwLint msg
checkRowContainsEff eff (TRowExtend tys row) msg = do
    case find matchesEff (map headTyCon tys) of
        Nothing -> checkRowContainsEff eff row msg
        Just _ -> pure ()
        where
            matchesEff (TCon con _) | con == eff = True
            matchesEff _ = False
checkRowContainsEff eff ty _ = throwLint $ "Trying to look up effect '" <> show eff <> "' in non-row type '" <> show ty <> "'"

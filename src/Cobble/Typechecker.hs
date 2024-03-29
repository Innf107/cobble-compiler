{-# LANGUAGE AllowAmbiguousTypes #-}
module Cobble.Typechecker (
    typecheck
,   TypeError(..)
,   TypeContext(..)

,   ppTC            -- TODO: Are these even strictly necessary?
,   ppTConstraint

,   TCEnv(..)
) where

import Cobble.Prelude hiding (subsume)
import Cobble.Util
import Cobble.Util.Bitraversable
import Cobble.Util.Polysemy.Fresh
import Cobble.Util.Polysemy.Dump
import Cobble.Util.Polysemy.Context
import Cobble.Util.TypeUtils
import Cobble.Syntax
import Cobble.Syntax qualified as C 

import qualified Data.Text as T

import qualified Data.Map as M
import Data.Traversable (for)

import qualified Data.Set as Set

import Data.List.NonEmpty qualified as NE

import Debug.Trace qualified as D

type NextPass = Codegen

data TCEnv = TCEnv {
    _varTypes :: M.Map QualifiedName Type
,   _tcInstances :: M.Map QualifiedName (Seq (Type, QualifiedName))
    -- ^ Once multiparam typeclasses are implemented, this will have to be @M.Map QualfiedName (Seq (Seq Type))@
,   _effOpTypes :: M.Map QualifiedName Type
} deriving (Show, Eq, Generic, Data)


data TypeError = DifferentTCon QualifiedName QualifiedName LexInfo (Seq TypeContext)
               | CannotUnify Type Type LexInfo (Seq TypeContext)
               | SkolBinding Type Type LexInfo (Seq TypeContext)
               | Occurs TVar Type LexInfo (Seq TypeContext)
               | Impredicative TVar Type LexInfo (Seq TypeContext)
               | NoInstanceFor Constraint LexInfo (Seq TypeContext)
               | InvalidRowHeadConstr Type LexInfo (Seq TypeContext)
               | RemainingRowFields (Seq (Type, QualifiedName, Seq Type)) Type Type LexInfo (Seq TypeContext)
               | MissingRowField QualifiedName Type Type LexInfo (Seq TypeContext)
               deriving (Show, Eq, Generic, Data)

data TypeContext = WhenUnifying Type Type
                 | WhenCheckingWanted Constraint
                 | InDefinitionFor QualifiedName Type
                 deriving (Show, Eq, Generic, Data)


throwType :: Members '[Error TypeError, Context TypeContext, Reader LexInfo] r
          => (LexInfo -> (Seq TypeContext) -> TypeError)
          -> Sem r a
throwType err = do
    li <- ask
    cxt <- getContext
    throw (err li cxt)

data TConstraint = MkTConstraint {
    getConstraint     :: TConstraintComp 
,   constraintLexInfo :: LexInfo
,   constraintContext :: Seq TypeContext
} deriving (Show, Eq, Generic, Data)

data TConstraintComp = ConUnify Type Type      -- σ ~ ρ
                     | ConWanted Constraint QualifiedName
                     --                     ^ dictionary variable
                     | ConGiven Constraint QualifiedName
                     --                    ^ dictionary
                     deriving (Show, Eq, Generic, Data)

(!~) :: Members '[Output TConstraint, Reader LexInfo, Context TypeContext] r 
     => Type 
     -> Type 
     -> Sem r ()
t1 !~ t2 = do
    li <- ask
    cxt <- getContext
    output (MkTConstraint (ConUnify t1 t2) li cxt)
infix 1 !~

subsume :: Members '[Output TConstraint, Reader LexInfo, Context TypeContext, Fresh Unique] r 
        => Type 
        -> Type 
        -> Sem r (Expr NextPass -> Expr NextPass)
subsume t1 t2 = do
    (t1', w) <- instantiate t1
    (t2', w') <- skolemize t2
    t1' !~ t2'
    pure (w' . w)

wanted :: Members '[Output TConstraint, Context TypeContext, Reader LexInfo] r 
    => Constraint
    -> QualifiedName
    -> Sem r ()
wanted c dv = do
    li <- ask
    cxt <- getContext
    output (MkTConstraint (ConWanted c dv) li cxt)

given :: Members '[Output TConstraint, Context TypeContext, Reader LexInfo] r 
    => Constraint
    -> QualifiedName
    -> Sem r ()
given c d = do
    li <- ask
    cxt <- getContext
    output (MkTConstraint (ConGiven c d) li cxt)

data Substitution = Subst {
        substVarTys :: Map TVar Type
    ,   substDicts  :: Map QualifiedName QualifiedName
    } 
    deriving stock   (Show, Eq, Generic, Data)

-- Not sure if this is really associative...
instance Semigroup Substitution where
    Subst s1 d1 <> Subst s2 d2 = 
        Subst 
            (M.filterWithKey notIdentical $ fmap (applySubst (Subst s2 d2)) s1 <> s2)
            (fmap (applySubst (Subst s2 d2)) d1 <> d2)
        where
            notIdentical tv (TUnif tv') | tv == tv' = False
            notIdentical _ _ = True

instance Monoid Substitution where
    mempty = Subst mempty mempty


{- Note [lookupType for VariantConstr]
For every pass after the Qualifier, Names are unique. 
Thus, a @VariantConstr@ and a simple @Var@ can never share the same name and
using the same Map for both is safe.
-}
lookupType :: HasCallStack => QualifiedName -> TCEnv -> Type
lookupType v TCEnv{_varTypes} = lookup v _varTypes & fromMaybe (error $ "lookupType: Typechecker cannot find variable: " <> show v)

insertType :: QualifiedName -> Type -> TCEnv -> TCEnv
insertType x t env@TCEnv{_varTypes} = env { _varTypes = insert x t _varTypes }

insertInstance :: QualifiedName -> Type -> QualifiedName -> TCEnv -> TCEnv
insertInstance className ty dictName env@TCEnv{_tcInstances} = env {
    _tcInstances = _tcInstances & flip alter className \case
    Nothing -> Just [(ty, dictName)]
    Just is -> Just (is :|> (ty, dictName))
}

insertEffOpType :: QualifiedName -> Type -> TCEnv -> TCEnv
insertEffOpType effName effTy env@TCEnv{_effOpTypes} = env { _effOpTypes = insert effName effTy _effOpTypes }

lookupEffOpType :: QualifiedName -> TCEnv -> Type
lookupEffOpType effName env = case lookup effName (_effOpTypes env) of
    Just ty -> ty
    Nothing -> error $ "lookupEffOpType: No such effect operation: " <> show effName

typecheck :: (Trace, Members '[Fresh Unique, Error TypeError, Context TypeContext, Dump (Seq TConstraint)] r)
          => TCEnv 
          -> Module Typecheck 
          -> Sem r (Module NextPass)
typecheck env (Module ext mname sts) = runReader (MkTagged mname) $ Module ext mname <$> typecheckStatements env sts

typecheckStatements :: (Trace, Members '[Fresh Unique, Error TypeError, Dump (Seq TConstraint), Reader (Tagged "ModName" Text)] r)
                    => TCEnv
                    -> (Seq (Statement Typecheck))
                    -> Sem r (Seq (Statement NextPass))
typecheckStatements env (st :<| sts) = do
    (constraints, (st', env')) <- runOutputSeq $ runContext $ typecheckStatement env st
    dump constraints
    subst <- solveConstraints (_tcInstances env') mempty constraints
    
    traceM TraceSubst (show subst)

    (applySubst subst st' <|) <$> typecheckStatements env' sts
typecheckStatements _env Empty = pure Empty

typecheckStatement :: (Trace, Members '[Fresh Unique, Output TConstraint, Reader (Tagged "ModName" Text), Context TypeContext] r)
                    => TCEnv
                    -> (Statement Typecheck)
                    -> Sem r (Statement NextPass, TCEnv)
typecheckStatement env (Def fixity li decl expectedTy) = runReader li do
    (decl', env') <- checkTopLevelDecl True env decl expectedTy
    pure (Def fixity li decl' expectedTy, env')

typecheckStatement env (DefClass k li cname tvs methSigs) = do
    -- We include the constraint and class bound tyvars on methods in the environment, 
    -- but not in the class definition since we shouldn't keep them around when lowering dictionary types.
    let env' = foldr (\(x, ty) env -> insertType x (TForall tv (addConstraint ty)) env) env methSigs
    pure (DefClass k li cname tvs methSigs, env')
        where 
            tv = case tvs of
                [tv] -> tv
                _ -> error "Multiparam type classes are NYI" 
            addConstraint (TForall tv ty) = TForall tv (addConstraint ty)
            addConstraint ty = TConstraint (MkConstraint cname k (TTyVar tv)) ty

typecheckStatement env (DefInstance (classKind, methDefs, params, _isImported) li cname ty meths) = runReader li do
    freshIX <- freshVar "" <&> \case
        UnsafeQualifiedName _ (GeneratedQName ix) -> ix
        qn -> error $ "freshVar returned non-generated qname: " <> show qn

    dictName <- UnsafeQualifiedName ("d_" <> (originalName cname) <> "_" <> show freshIX) . GlobalQName <$> asks unTagged
    let env' = insertInstance cname ty dictName env

    meths' <- forM (zip meths methDefs) \(decl@(Decl _ declName _ _), (methName, methTy)) -> do
        -- sanity check
        when (methName /= declName) $ error $ "typecheckStatement: instance methods were not properly reordered at " <> show li <> "."
        
        let methTy' = case params of
                [p] -> replaceTyVar p ty methTy
                _ -> error "typecheckStatement: multi parameter typeclasses NYI"

        traceM TraceTC $ "[typecheckStatement (instance " <> show cname <> " " <> show ty <> ")]: Γ ⊢ " <> show methName <> " : " <> show methTy <> " ====> " <> show methTy'

        -- We discard the modified environment, since instance methods should really not be able to modify it.
        fst <$> checkTopLevelDecl False env' decl methTy'
    pure (DefInstance (classKind, methDefs, params, dictName) li cname ty meths', env')
        where
            stripConstraint c (TForall tv ty) = TForall tv (stripConstraint c ty)
            stripConstraint c (TConstraint c' ty) | c == c' = ty
            stripConstraint _c _ = error $ "typecheckStatement: non-constrained instance method at " <> show li

typecheckStatement env (DefVariant k li tyName tvs constrs) = do
    let resTy = foldl' (\x y -> TApp x (TTyVar y)) (TCon tyName k) tvs
    let constrTy ps = forallFun tvs resTy ps
    constrs' <- traverse (\(n, ps, (i, j)) -> (\psTy -> (n, ps, (psTy, i, j))) <$> constrTy ps) constrs
    let env' = foldr (\(n,_,(t,_,_)) -> insertType n t) env constrs'

    pure (DefVariant k li tyName tvs constrs', env')
        where
typecheckStatement env (DefEffect k li effName tvs ops) = do
    let env' = foldr insertOpTy env ops

    pure (DefEffect k li effName tvs ops, env')
        where
            insertOpTy (opName, ty) env = insertEffOpType opName (tforall tvs ty) $ insertType opName (tforall tvs ty) env 

tforall :: Seq TVar -> Type -> Type
tforall = flip (foldr TForall)

-- | Creates an effect-polymorphic function type consisting of the type variables @vars@ and
-- the argument types @args@ and a return type @result@
forallFun :: Members '[Fresh Unique] r => Seq TVar -> Type -> Seq Type -> Sem r Type
forallFun vars result args = do
    argsAndEffs <- traverse (\x -> freshVar "μ" <&> \effName -> (x, MkTVar effName (KRow KEffect))) args
    pure $ tforall (vars <> map snd argsAndEffs) $ foldr (\(x, eff) r -> TFun x (TTyVar eff) r) result argsAndEffs

checkTopLevelDecl :: 
      ( Trace
      , Members 
      '[ Fresh Unique
       , Output TConstraint
       , Reader LexInfo
       , Context TypeContext
       ] r)
      => Bool
      -> TCEnv
      -> Decl Typecheck
      -> Type
      -> Sem r (Decl NextPass, TCEnv)
checkTopLevelDecl recursive env (Decl () f xs e) expectedTy = withContext (InDefinitionFor f expectedTy) do
    li <- ask
    (expectedTy', w) <- skolemize expectedTy
    (xs', eTy, mEff, _w') <- decomposeParams expectedTy' xs
    traceM TraceTC $ "[checkTopLevelDecl env (" <> show f <> ")]\n    expectedTy = " <> show expectedTy <> "\n    expectedTy' = " <> show expectedTy' <> "\n    xs' = " <> show xs' <> " | mEff = " <> show mEff <> " | eTy = " <> show eTy

    let env' = if recursive
               then insertType f expectedTy env
               else env

    let eff = case mEff of
            Just e -> e
            -- Top level definitions have to be pure. This leaves the door open to potentially evaluating them lazily
            -- in the future?
            Nothing -> TRowClosed []

    e' <- runReader (MkTagged Nothing) $ check (foldr (\(x,ty,_) -> insertType x ty) env' xs') e eTy eff
    let lambdas = w $ makeLambdas li e' xs'

    pure (Decl expectedTy f [] lambdas, env')
        where
            makeLambdas :: LexInfo -> Expr NextPass -> Seq (QualifiedName, Type, Effect) -> Expr NextPass
            makeLambdas li = foldr (\(x, ty, eff) e -> Lambda (TFun ty eff (getType e), ty, eff) li x e)

check :: 
    ( Trace, 
      Members 
      '[ Output TConstraint
       , Fresh Unique
       , Context TypeContext
       , Reader (Tagged "resumeTy" (Maybe (Type, Type)))
       --                                  ^     ^ resume result
       --                                  | resume arg
       ] r)
      => TCEnv
      -> Expr Typecheck 
      -> Type 
      -> Effect
      -> Sem r (Expr NextPass)
check _env e t eff | trace TraceTC ("[check]: Γ ⊢ " <> show e <> " : " <> ppType t <> " | " <> ppType eff) False = error "unreachable"
-- We need to 'correct' the extension field to resubstitute the skolems that were introduced by skolemize
check env e t@TForall{} eff = runReader (getLexInfo e) do
    (t', w) <- skolemize t
    w . correct t <$> check env e t' eff

check env (App () li f x) t eff = runReader li do
    (f', fValEff) <- infer env f
    (fDomTy, fFunEff, fCodomTy, w) <- decomposeFun (getType f')
    fValEff !~ fFunEff
    fValEff !~ eff
    traceM TraceTC $ "[check env (App ...)] getType f' = " <> ppType (getType f') <> " | t = " <> ppType t <> " | fDomTy = " <> ppType fDomTy  <> " | fFunEff = " <> ppType fFunEff <> " | fCodomTy = " <> ppType fCodomTy
    
    x' <- check env x fDomTy eff

    w' <- subsume fCodomTy t

    pure $ w' (App t li (w f') x')

check _env (IntLit () li n) t _eff = runReader li $ do
    t !~ intT 
    pure (IntLit () li n)

check env (If () li c th el) t eff = do
    c' <- check env c boolT eff
    th' <- check env th t eff
    el' <- check env el t eff
    pure (If t li c' th' el')

check env (Let () li (Decl () f xs e1) e2) t eff = runReader li do
    xs' <- traverse (\x -> (x,) <$> freshUnif KStar) xs

    (e1', e1Eff) <- infer (foldr (uncurry insertType) env xs') e1

    eff !~ e1Eff

    Let () li (Decl (getType e1') f xs' e1') <$> check (insertType f (getType e1') env) e2 t eff


check env (Var () li x) t _eff = runReader li do
    let xTy = lookupType x env
    w <- subsume xTy t
    pure $ w (Var t li x)

check env (Ascription () li e (coercePass -> t1)) t2 eff = runReader li do
    e' <- check env e t1 eff
    w <- subsume t1 t2
    pure $ w e'

check env (VariantConstr (_i, j) li c) t _eff = runReader li do
    let cTy = lookupType c env
    w <- subsume cTy t
    pure $ w (VariantConstr (t, cTy, j) li c)

check env (Case () li e branches) t eff = runReader li do
    (e', eEff) <- infer env e
    eEff !~ eff
    branches' <- forM branches \(CaseBranch () li p brExpr) -> runReader li do
        (p', extendEnv) <- checkPattern env p (getType e')
        brExpr' <- check (extendEnv env) brExpr t eff
        pure (CaseBranch () li p' brExpr')
    pure $ Case t li e' branches'

check env (Lambda () li x e) t _eff = runReader li do
    (expectedArgTy, tEff, expectedResTy, w) <- decomposeFun t
    traceM TraceTC $ "[check env (Lambda ...)] t = " <> ppType t <> " | expectedArgTy = " <> ppType expectedArgTy <> " | expectedResTy = " <> ppType expectedResTy

    -- We check the expression against the effect contained in its type, *not* against
    -- the environment effect (which is irrelevant since lambdas are values)
    e' <- check (insertType x expectedArgTy env) e expectedResTy tEff 

    pure $ w (Lambda (t, expectedArgTy, tEff) li x e')
check env (Handle () li scrut handlers mreturnClause) t eff = runReader li do
    -- Check handlers
    (handlerEffs, handlers') <- unzip <$> forM handlers \(EffHandler () li opName args e) -> runReader li do
        let opType = lookupEffOpType opName env

        -- TODO: What should we do about w here?
        (argsWithEffs', resTy, mresEff, _w) <- decomposeParams opType args

        -- _ <- error (show (w (IntLit () InternalLexInfo 5)))

        let args' = map (\(x, ty, _) -> (x, ty)) argsWithEffs'
        
        let resEff = case mresEff of
                Nothing -> error $ "parameterless effect operation in handle expressions"
                Just resEff -> resEff

        let env' = foldr (uncurry insertType) env args'

        -- Handler expressions run in the *parent's* effect context (obviously)
        e' <- runReader (MkTagged @"resumeTy" (Just (resTy, t))) $ check env' e t eff

        traceM TraceTC $ "[check handle '" <> show opName <> "']: opType = " <> show opType <> " | resEff = " <> show resEff

        pure (resEff, EffHandler (removeRow resEff) li opName args' e')

    handlerEff <- case (map rowHead handlerEffs) of
        (e1 :<| effs) -> foldrM (\eff1 eff2 -> (eff1 !~ eff2) $> eff2) e1 effs
        Empty -> error $ "check ... (Handle ...): empty handler list"


    -- We have to infer the scrutinee, since the actual return type
    -- might be changed by a return clause
    (scrut', scrutEff) <- infer env scrut

    scrutEff !~ rowInsert handlerEff eff

    traceM TraceTC $ "[check (Handle ...)]: handlerEffs = " <> show handlerEffs <> " | handlerEff = " <> show handlerEff <> " | eff = " <> show eff

    -- Check the return clause
    (w, mreturnClause') <- case mreturnClause of
        Nothing -> do
            -- If there is no return clause, the result of the expression itself
            -- has to match the type we are matching against
            w <- subsume (getType scrut') t
            pure (w, Nothing)
        Just (var, expr) -> do
            
            let env' = insertType var (getType scrut') env

            expr' <- check env' expr t eff

            pure (id, Just (var, expr'))

        
    pure (w (Handle (t, handlerEff) li scrut' handlers' mreturnClause'))


check env (Resume () li arg) t eff = runReader li do
    (resumeArgTy, resumeResTy) <- fromMaybe (error "resume outside of handle expr") <$> asks unTagged
    
    w <- subsume resumeResTy t

    arg' <- check env arg resumeArgTy eff

    pure $ w (Resume t li arg')

checkPattern :: Members '[Output TConstraint, Fresh Unique, Reader LexInfo, Context TypeContext] r 
             => TCEnv
             -> Pattern Typecheck
             -> Type
             -> Sem r (Pattern NextPass, TCEnv -> TCEnv)
checkPattern env pats ty = evalState mempty $ go Nothing env pats ty
    where
        go :: Members '[Output TConstraint, Fresh Unique, Reader LexInfo, Context TypeContext, State (Map QualifiedName Type)] r
           => Maybe (Map QualifiedName Type)
           -> TCEnv
           -> Pattern Typecheck
           -> Type
           -> Sem r (Pattern NextPass, TCEnv -> TCEnv)
        go _ _env (IntP () n) _t = pure (IntP () n, id)
        
        go Nothing _env (VarP () x) t = do 
            modify (insert x t)
            pure (VarP t x, insertType x t)
        
        go (Just orPatTys) _env (VarP () x) t = case lookup x orPatTys of
            Nothing -> error $ "checkPattern: variable '" <> show x <> "' not bound by or pattern. orPatTys=" <> show orPatTys
            Just ty -> do
                t !~ ty -- TODO: Use subsumption?
                pure (VarP t x, id)

        go mOrPatTys env (ConstrP (i,v) cname ps) t = do
            (constrTy, _w) <- instantiate (lookupType cname env)
            (typedPats, resTy, _meff, _w') <- decomposeParams constrTy ps
            -- TODO: I don't know...
            resTy !~ t
            (ps', exts) <- unzip <$> forM typedPats \(p, pTy, _eff) -> go mOrPatTys env p pTy
            pure (ConstrP (t,i,v) cname ps', foldr (.) id exts)
        go _ _env (WildcardP _) t = pure (WildcardP t, id)
        go mOrPatTys env (OrP () (p :<| pats)) t = do
            (boundTys, (p', pWrapper)) <- runState mempty $ go mOrPatTys env p t
            
            (pats', wrappers) <- unzip <$> traverse (flip (go (mOrPatTys <> Just boundTys) env) t) pats

            pure (OrP (getType p') (p' :<| pats'), foldr (.) pWrapper wrappers)
        go _ _ (OrP () Empty) _ = error "checkPattern: Empty Or-Pattern"

infer :: 
    (Trace, 
     Members 
     '[ Output TConstraint
      , Fresh Unique
      , Context TypeContext
      , Reader (Tagged "resumeTy" (Maybe (Type, Type)))
      ] r )
      => TCEnv
      -> Expr Typecheck 
      -> Sem r (Expr NextPass, Effect)
infer env (App () li f x) = runReader li do
    (f', fValEff) <- infer env f
    (fDomTy, fFunEff, fCodomTy, wF) <- decomposeFun (getType f') 
    traceM TraceTC $ "[infer env (App ...)] getType f' = " <> ppType (getType f') <> " | fDomTy = " <> ppType fDomTy <> " | fFunEff = " <> ppType fFunEff <> " | fCodomTy = " <> ppType fCodomTy
    fValEff !~ fFunEff

    x' <- check env x fDomTy fFunEff

    (t, w) <- instantiate fCodomTy

    pure (w (App t li (wF f') x'), fFunEff)
infer _env (IntLit () li n) = do
    eff <- freshEffectRow
    pure (IntLit () li n, eff)
infer env (If () li c th el) = runReader li do
    -- This is hard, since we have to make sure th and el have the same type,
    -- but we cannot simply use (!~), which could not infer higher rank types.
    -- Instead, we check that both types are equivalent using subsumption:
    --     getType th' <= getType el' /\ getType el' <= getType th'.
    (c', cEff)  <- infer env c
    (th', thEff) <- infer env th
    (el', elEff) <- infer env el

    -- Effects cannot be polytypes, so (!~) is fine here.
    cEff !~ thEff
    cEff !~ elEff

    getType c' !~ boolT

    -- Make sure that the types of th' and el' are
    -- equivalent (though they may not necessarily *look* the same).
    w1 <- subsume (getType th') (getType el')
    w2 <- subsume (getType el') (getType th')

    -- We arbitrarily choose getType th' as the final type, since the types of
    -- th' and el' are equivalent.
    pure (If (getType th') li c' (w1 th') (w2 el'), cEff)

infer env (Let () li (Decl () f xs e1) e2) = runReader li do
    xs' <- traverse (\x -> (x,) <$> freshUnif KStar) xs

    (e1', e1Eff) <- infer (foldr (uncurry insertType) env xs') e1
    (e2', e2Eff) <- infer (insertType f (getType e1') env) e2

    e1Eff !~ e2Eff

    pure (Let () li (Decl (getType e1') f xs' e1') e2', e1Eff)

infer env (Var () li x) = runReader li do
    let xTy = lookupType x env
    (ty, w) <- instantiate xTy

    eff <- freshEffectRow
    pure (w (Var ty li x), eff)

infer env (Ascription () li e t) = runReader li do
    eff <- freshEffectRow
    e' <- check env e t eff
    (_t2, w) <- instantiate t
    pure (w e', eff)


infer env (VariantConstr (_i, j) li c) = runReader li do
    let cTy = lookupType c env
    (cTy', w) <- instantiate cTy
    
    eff <- freshEffectRow
    pure (w $ VariantConstr (cTy', cTy, j) li c, eff)

infer env (Case () li e branches) = runReader li do
    (e', _eEff) <- infer env e
    branches' <- forM branches \(CaseBranch () li p brExpr) -> do
        (p', extendEnv) <- checkPattern env p (getType e')
        (brExpr', brEff) <- infer (extendEnv env) brExpr
        pure (CaseBranch () li p' brExpr', brEff)
    

    -- We have to make sure all branches have the same return type.
    -- See 'infer env (If ...)' for a more detailed explanation
    (t, eff) <- checkEquiv (map (\(CaseBranch _ _ _ expr, eff) -> (getType expr, eff)) branches')
    pure (Case t li e' (map fst branches'), eff)
    where
        checkEquiv :: forall r. Members '[Reader LexInfo, Output TConstraint, Fresh Unique, Context TypeContext] r 
                   => Seq (Type, Effect) 
                   -> Sem r (Type, Effect)
        checkEquiv Empty                = (,) <$> freshUnif KStar <*> freshEffectRow -- the case expression is empty.
        checkEquiv ((t, eff) :<| Empty) = pure (t, eff)
        checkEquiv ((t1, eff1):<|(t2, eff2):<|ts) = do
            _w1 <- subsume t1 t2 -- TODO: apply wrapping?
            _w2 <- subsume t2 t1
            eff1 !~ eff2
            checkEquiv ((t2, eff2)<|ts)


infer env (Lambda () li x e) = do
    xTy <- freshUnif KStar

    (e', eEff) <- infer (insertType x xTy env) e

    -- The lambda captures the body's effect in the function type, but it itself has a fully polymorphic
    -- effect type.
    lamEff <- freshEffectRow 
    pure (Lambda (TFun xTy eEff (getType e'), xTy, lamEff) li x e', lamEff)
infer env expr@(Handle () li _ _ _) = runReader li do
    -- Defer to checking against a type variable here. TODO: I think this cannot infer polytypes, so that might be an issue
    resTy <- freshUnif KStar
    resEff <- freshEffectRow
    expr' <- check env expr resTy resEff
    pure (expr', resEff)

infer env (Resume () li arg) = do
    (resumeArgTy, resumeResTy) <- fromMaybe (error "resume outside of handle expr") <$> asks unTagged
    
    eff <- freshEffectRow
    
    arg' <- check env arg resumeArgTy eff
    
    pure (Resume resumeResTy li arg', eff)


correct :: Type -> Expr NextPass -> Expr NextPass
correct = setType 

{- note: [Generalization]
We don't perform *any* (implicit) generalization at the moment.
Top level functions have to include type signatures anyway and generalization for
local bindings is known to cause problems and is rarely that useful[1].
In the few cases where polymorphic lets are useful, it is always possible to write an explicit ascription.

Generalization for top level functions becomes quite limited as more complicated type system features are introduced and,
since even in Haskell, function signatures are strongly encouraged, it is ultimately not that useful.

[1]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf
-}



instantiate :: Members '[Fresh Unique, Reader LexInfo, Output TConstraint, Context TypeContext] r 
            => Type 
            -> Sem r (Type, Expr NextPass -> Expr NextPass)
instantiate (TForall tv ty) = ask >>= \li -> do
    unif <- TUnif <$> freshTVar tv
    (ty', w) <- instantiate (replaceTyVar tv unif ty)
    pure (ty', w . TyApp li unif)
instantiate (TConstraint c ty) = do
    li <- ask
    dictVar <- freshVar "dv"
    wanted c dictVar
    (ty', w) <- instantiate ty
    pure (ty', \e -> w $ DictVarApp li e dictVar)
instantiate ty = do
    pure (ty, id)


decomposeFun :: Members '[Fresh Unique, Reader LexInfo, Output TConstraint, Context TypeContext] r 
          => Type 
          -> Sem r (Type, Effect, Type, Expr NextPass -> Expr NextPass)
decomposeFun t@TForall{} = do
    (t', w) <- instantiate t
    (x, eff, y, w') <- decomposeFun t'
    pure (x, eff, y, w . w')

decomposeFun (TFun a eff b) = pure (a, eff, b, id)
decomposeFun t = do
    argTy <- freshUnif KStar
    effTy <- freshEffectRow
    resTy <- freshUnif KStar
    w <- subsume t (TFun argTy effTy resTy)
    pure (argTy, effTy, resTy, w)

decomposeParams :: Members '[Fresh Unique, Reader LexInfo, Output TConstraint, Context TypeContext] r
                => Type
                -> Seq a
                -> Sem r (Seq (a, Type, Effect), Type, Maybe Effect, Expr NextPass -> Expr NextPass)
decomposeParams ty Empty = pure ([], ty, Nothing, id)
decomposeParams ty (x :<| xs) = do
    (argTy, eff, resTy, w) <- decomposeFun ty 
    (restArgTys, restResTy, mEff, w') <- decomposeParams resTy xs
    pure ((x,argTy, eff) <| restArgTys, restResTy, mEff <|> Just eff, w . w')

freshEffectRow :: Members '[Fresh Unique] r => Sem r Type
freshEffectRow = freshUnif (KRow KEffect)

freshUnif :: Members '[Fresh Unique] r => Kind -> Sem r Type
freshUnif k = freshVar "u" <&> \u -> TUnif (MkTVar u k) 

rowInsert :: Type -> Type -> Type
rowInsert ty (TRowUnif tys var) = TRowUnif (ty <| tys) var
rowInsert ty (TRowVar tys var) = TRowVar (ty <| tys) var
rowInsert ty (TRowClosed tys) = TRowClosed (ty <| tys)
rowInsert ty (TRowSkol tys var skol) = TRowSkol (ty <| tys) var skol
rowInsert ty (TTyVar tv)     = TRowVar [ty] tv
rowInsert ty (TUnif tv)      = TRowVar [ty] tv
rowInsert ty (TSkol tv skol) = TRowSkol [ty] tv skol
rowInsert ty rowTy = error $ "rowInsert: Trying to insert into non-row type: " <> show rowTy <> "\n    Type to insert: " <> show ty

-- | @rowExtend@ extends the second row with the
-- types from the first one, disregarding the first ones row variable.
-- It therefore has the same effect as instantiating the first type variable
-- with the second row type *IFF* the first type variable is not used anywhere else.
_rowExtend :: HasCallStack => Type -> Type -> Type
_rowExtend (TRowUnif tys1 _) (TRowVar tys2 var2)        = TRowVar (tys1 <> tys2) var2
_rowExtend (TRowUnif tys1 _) (TRowUnif tys2 var2)       = TRowUnif (tys1 <> tys2) var2
_rowExtend (TRowUnif tys1 _) (TRowClosed tys2)          = TRowClosed (tys1 <> tys2)
_rowExtend (TRowUnif tys1 _) (TRowSkol tys2 skol2 var2) = TRowSkol (tys1 <> tys2) skol2 var2
_rowExtend (TRowUnif tys1 _) (TTyVar var2)              = TRowVar tys1 var2
_rowExtend (TRowUnif tys1 _) (TUnif var2)               = TRowUnif tys1 var2
_rowExtend (TRowUnif tys1 _) (TSkol skol2 var2)         = TRowSkol tys1 skol2 var2
_rowExtend t1 t2 = error $ "rowExtend: Trying to extend invalid types:\n    t1: " <> show t1 <> "\n    t2: " <> show t2

rowHead :: HasCallStack => Type -> Type
rowHead (TRowVar (ty :<| _) _) = ty
rowHead (TRowUnif (ty :<| _) _) = ty
rowHead (TRowClosed (ty :<| _)) = ty
rowHead (TRowSkol (ty :<| _) _ _) = ty
rowHead ty = error $ "rowHead: Not a non-empty row type: " <> show ty

_replaceUnif :: TVar -> Type -> Type -> Type
_replaceUnif tv ty = replaceUnifs (one (tv, ty))

replaceUnifs :: HasCallStack => Map TVar Type -> Type -> Type
replaceUnifs _ ty@TTyVar{} = ty
replaceUnifs tvs ty@(TUnif tv) = case lookup tv tvs of
                Just ty' -> ty'
                Nothing -> ty
replaceUnifs _ ty@TCon{} = ty
replaceUnifs _ ty@TSkol{} = ty
replaceUnifs tvs (TApp t1 t2) = TApp (replaceUnifs tvs t1) (replaceUnifs tvs t2)
replaceUnifs tvs (TFun a eff b) = TFun (replaceUnifs tvs a) (replaceUnifs tvs eff) (replaceUnifs tvs b)
replaceUnifs tvs (TForall forallTV ty) =
                let remainingTVs = M.delete forallTV tvs in
                TForall forallTV $ replaceUnifs remainingTVs ty
replaceUnifs tvs (TConstraint (MkConstraint constrName k constrTy) ty) =
                TConstraint (MkConstraint constrName k (replaceUnifs tvs constrTy)) (replaceUnifs tvs ty)
replaceUnifs tvs (TRowClosed tys)   = TRowClosed (map (replaceUnifs tvs) tys)
replaceUnifs tvs (TRowUnif tys var) = let replacedTys = map (replaceUnifs tvs) tys in case lookup var tvs of
    Nothing                         -> TRowUnif replacedTys var
    Just (TTyVar var')              -> TRowVar replacedTys var'
    Just (TUnif var')               -> TRowUnif replacedTys var'
    Just (TSkol skol var')          -> TRowSkol replacedTys skol var'
    Just (TRowClosed tys')          -> TRowClosed (replacedTys <> tys')
    Just (TRowUnif tys' var')       -> TRowUnif (replacedTys <> tys') var'
    Just (TRowVar tys' var')        -> TRowVar (replacedTys <> tys') var'
    Just (TRowSkol tys' skol var')  -> TRowSkol (replacedTys <> tys') skol var'
    Just ty -> error $ "replaceUnifs: Trying to replace variable '" <> show var <> "' in open row type '" <> show (TRowUnif tys var)
                        <> "' with non-row type: " <> show ty
replaceUnifs tvs (TRowVar tys var) = TRowVar (map (replaceUnifs tvs) tys) var
replaceUnifs tvs (TRowSkol tys skol var) = TRowSkol (map (replaceUnifs tvs) tys) skol var

replaceTyVar :: TVar -> Type -> Type -> Type
replaceTyVar tv ty = replaceTyVars (one (tv, ty))

replaceTyVars :: HasCallStack => Map TVar Type -> Type -> Type
replaceTyVars tvs ty@(TTyVar tv) = case lookup tv tvs of
                Just ty' -> ty'
                Nothing -> ty
replaceTyVars _tvs ty@TUnif{} = ty
replaceTyVars _tvs ty@TCon{} = ty
replaceTyVars _tvs ty@TSkol{} = ty
replaceTyVars tvs (TApp t1 t2) = TApp (replaceTyVars tvs t1) (replaceTyVars tvs t2)
replaceTyVars tvs (TFun a eff b) = TFun (replaceTyVars tvs a) (replaceTyVars tvs eff) (replaceTyVars tvs b)
replaceTyVars tvs (TForall forallTV ty) =
                let remainingTVs = M.delete forallTV tvs in
                TForall forallTV $ replaceTyVars remainingTVs ty
replaceTyVars tvs (TConstraint (MkConstraint constrName k constrTy) ty) =
                TConstraint (MkConstraint constrName k (replaceTyVars tvs constrTy)) (replaceTyVars tvs ty)
replaceTyVars tvs (TRowClosed tys)   = TRowClosed (map (replaceTyVars tvs) tys)
replaceTyVars tvs row@(TRowVar tys var) = let replacedTys = map (replaceTyVars tvs) tys in case lookup var tvs of
    Nothing                         -> TRowUnif replacedTys var
    Just (TTyVar var')              -> TRowVar replacedTys var'
    Just (TUnif var')               -> TRowUnif replacedTys var'
    Just (TSkol skol var')          -> TRowSkol replacedTys skol var'
    Just (TRowClosed tys')          -> TRowClosed (replacedTys <> tys')
    Just (TRowUnif tys' var')       -> TRowUnif (replacedTys <> tys') var'
    Just (TRowVar tys' var')        -> TRowVar (replacedTys <> tys') var'
    Just (TRowSkol tys' skol var')  -> TRowSkol (replacedTys <> tys') skol var'
    Just ty -> error $ "replaceTVars: Trying to replace variable '" <> show var <> "' in open row type '" <> show row
                        <> "' with non-row type: " <> show ty
replaceTyVars tvs (TRowUnif tys var) = TRowUnif (map (replaceTyVars tvs) tys) var
replaceTyVars tvs (TRowSkol tys skol var) = TRowSkol (map (replaceTyVars tvs) tys) skol var

solveConstraints :: (Trace, Members '[Error TypeError, Fresh Unique] r)
                 => Map QualifiedName (Seq (Type, QualifiedName))
                 --                              v^dict
                 -> Seq (Constraint, QualifiedName, LexInfo, Seq TypeContext)
                 -> Seq TConstraint
                 -> Sem r Substitution
solveConstraints givens wanteds Empty = solveWanteds givens wanteds
solveConstraints givens wanteds ((MkTConstraint (ConUnify t1 t2) li cxt) :<| constrs) = runReader li do
            -- This is a bit inefficient (quadratic?).
            -- Let's make sure the constraint solver actually works, before we try to deal with that.
            subst <- runContextInitial cxt $ unifyCxt t1 t2
            (subst <>) <$> solveConstraints (applySubst subst givens) (applySubst subst wanteds) (applySubst subst constrs)
solveConstraints givens wanteds ((MkTConstraint (ConWanted c dictVar) li cxt) :<| constrs) = runReader li do
            solveConstraints givens ((c, dictVar, li, cxt) :<| wanteds) constrs
        
solveConstraints givens wanteds ((MkTConstraint (ConGiven (MkConstraint cname _k ty) dict) li _cxt) :<| constrs) = runReader li do
            let givens' = alter (<> Just [(ty, dict)]) cname givens
            solveConstraints givens' wanteds constrs

solveWanteds :: (Trace, Members '[Error TypeError, Fresh Unique] r)
                => Map QualifiedName (Seq (Type, QualifiedName))
                 --                              v^dict
                -> Seq (Constraint, QualifiedName, LexInfo, Seq TypeContext)
                -> Sem r Substitution
solveWanteds _givens Empty = pure mempty
solveWanteds givens ((c@(MkConstraint cname _k ty), dictVar, li, cxt) :<| wanteds) = 
    runContextInitial cxt $ runReader li $ withContext (WhenCheckingWanted c) do
        case lookup cname givens of
            Just tys -> do
                let trySolve Empty = throwType $ NoInstanceFor c
                    trySolve ((t2, d) :<| tys) = do
                        runError @TypeError (unifyCxt ty t2) >>= \case
                            Left err -> do
                                traceM TraceSolver $ "[solveWanteds] [W] " <> show c <> ": " <> show ty <> " ~ " <> show t2 <> ": " <> show err
                                trySolve tys
                            Right subst -> do
                                traceM TraceSolver $ "[solveWanteds] [W] " <> show c <> ": " <> show ty <> " ~ " <> show t2 <> " ✓"
                                pure $ subst <> Subst mempty (one (dictVar, d)) -- Substitute the dictionary
                -- We can rely on coherence and non-overlapping instances here and just pick the
                -- first matching dictionary that we find.
                -- TODO: Should we throw an ambiguity error if subst contains non-empty tyvar substitutions?
                subst <- trySolve tys
                (subst <>) <$> solveWanteds (applySubst subst givens) (applySubst subst wanteds)
            Nothing -> throwType $ NoInstanceFor c

unifyCxt :: (Trace, Members '[Error TypeError, Context TypeContext, Reader LexInfo, Fresh Unique] r)
         => Type
         -> Type
         -> Sem r Substitution
unifyCxt t1 t2 = withContext (WhenUnifying t1 t2) $ unify t1 t2

unify :: (Trace, Members '[Error TypeError, Context TypeContext, Reader LexInfo, Fresh Unique] r)
      => Type
      -> Type
      -> Sem r Substitution
unify t1 t2 | trace TraceUnify ("[unify]: " <> show t1 <> " ~ " <> show t2) False = error "unreachable"
unify (TCon c1 _) (TCon c2 _)
    | c1 == c2 = pure mempty
    | otherwise = throwType $ DifferentTCon c1 c2
unify (TUnif tv) t2 = bind tv t2
unify t1 (TUnif tv) = bind tv t1
unify (TApp a1 b1) (TApp a2 b2) = do
    subst <- unify a1 a2
    (subst <>) <$> unify (applySubst subst b1) (applySubst subst b2)
unify (TFun a1 eff1 b1) (TFun a2 eff2 b2) = do
    subst <- unify a1 a2
    subst' <- (subst <>) <$> unify (applySubst subst eff1) (applySubst subst eff2)
    (subst' <>) <$> unify (applySubst subst' b1) (applySubst subst' b2)
unify t1@TSkol{} t2@TSkol{}
    | t1 == t2 = pure mempty
unify (TForall tv1 ty1) (TForall tv2 ty2) = do
        -- We unify foralls up to α-equivalence by substituting the forall variables in both polytypes
        -- with a shared fresh skolem (We arbitrarily build this skolem out of the variable from @poly1@)
        skol <- freshSkol tv1
        unify (replaceTyVar tv1 skol ty1) (replaceTyVar tv2 skol ty2)

-- Open and skolem rows with an empty extension should be equivalent to the variables/skolems on their own.
unify (TRowUnif Empty var) t2 = unify (TUnif var) t2
unify (TRowSkol Empty skol var) t2 = unify (TSkol skol var) t2
unify t1 (TRowUnif Empty var) = unify t1 (TUnif var)
unify t1 (TRowSkol Empty skol var) = unify t1 (TSkol skol var)

unify row1@(TRowClosed t1s) row2@(TRowClosed t2s) = 
    unifyRows row1 row2 t1s t2s \case
        [] -> pure mempty
        remaining -> throwType $ RemainingRowFields remaining row1 row2
unify row1@(TRowUnif t1s tvar) row2@(TRowClosed t2s) = 
    unifyRows row1 row2 t1s t2s 
        \remaining -> bind tvar (TRowClosed (map (\(ty,_,_) -> ty) remaining))
unify row1@TRowClosed{} row2@TRowUnif{} = unify row2 row1
unify row1@(TRowUnif t1s tvar) row2@(TRowSkol t2s skol tvar2) = 
    unifyRows row1 row2 t1s t2s \case
        [] -> bind tvar (TSkol skol tvar2)
        remaining -> bind tvar (TRowSkol (map (\(ty,_,_) -> ty) remaining) skol tvar2)
unify row1@TRowSkol{} row2@TRowUnif{} = unify row2 row1

unify row1@(TRowSkol t1s skol1 var1) row2@(TRowSkol t2s skol2 var2)
    | skol1 == skol2 && var1 == var2 = unifyRows row1 row2 t1s t2s \case
        [] -> pure mempty
        remaining -> throwType $ RemainingRowFields remaining row1 row2 


unify (TRowUnif t1s tvar1@(MkTVar _ kind)) (TRowUnif t2s tvar2) = do
    headT1s <- traverse (\ty -> (\(name, tys) -> (name, (tys, ty))) <$> getHeadConstr ty) t1s
    headT2s <- traverse (\ty -> (\(name, tys) -> (name, (tys, ty))) <$> getHeadConstr ty) t2s
    go headT1s headT2s []
        where
            go ((con1, (args1, ty1)) :<| t1s) t2s remaining1 = do
                case lookupAndDelete con1 t2s of
                    Nothing -> go t1s t2s (ty1 <| remaining1) -- TODO: prepend or append? This is probably important with duplicate labels.
                    Just ((args2, _), t2s') -> do
                        subst <- unifyAll args1 args2
                        (subst<>) <$> go (applySubst subst t1s) (applySubst subst t2s') remaining1
            go Empty remaining2 remaining1 = do
                newRowVar <- freshVar "μ" <&> \mu -> MkTVar mu kind
                (<>)
                    <$> bind tvar1 (TRowUnif (map (\(_,(_,ty)) -> ty) remaining2) newRowVar) 
                    <*> bind tvar2 (TRowUnif remaining1 newRowVar)

unify row1@TRowClosed{} row2@TRowSkol{} = throwType $ CannotUnify row1 row2
unify row1@TRowSkol{} row2@TRowClosed{} = throwType $ CannotUnify row1 row2

unify row@TRowVar{} ty = error $ "Uninstantiated TRowVar in unification: '" <> show row <> " ~ " <> show ty <> "'"
unify ty row@TRowVar{} = error $ "Uninstantiated TRowVar in unification: '" <> show ty <> " ~ " <> show row <> "'"
unify var@TTyVar{} ty  = error $ "Uninstantiated TTyVar in unification: '" <> show var <> " ~ " <> show ty <> "'"
unify ty var@TTyVar{}  = error $ "Uninstantiated TTyVar in unification: '" <> show ty <> " ~ " <> show var <> "'"

unify s@TSkol{} t2  = throwType $ SkolBinding s t2
unify t1 s@TSkol{}  = throwType $ SkolBinding t1 s
unify t1 t2         = throwType $ CannotUnify t1 t2 

unifyRows :: (Trace, Members '[Reader LexInfo, Error TypeError, Context TypeContext, Fresh Unique] r)
          => Type 
          -> Type
          -> (Seq Type)
          -> (Seq Type)
          -> (Seq (Type, QualifiedName, Seq Type) -> Sem r Substitution)
          -> Sem r Substitution
unifyRows row1 row2 t1s t2s remainingCont = do
    headT1s <- traverse getHeadConstr t1s
    headT2s <- traverse (\ty -> (\(name, tys) -> (name, (tys, ty))) <$> getHeadConstr ty) t2s
    traceM TraceUnify $ "[unifyRows]: Unifying head constructors " <> show headT1s <> " and " <> show headT2s 
    go headT1s headT2s
        where
            go ((con1, args1) :<| t1s) t2s = do
                case lookupAndDelete con1 t2s of
                    Nothing -> throwType $ MissingRowField con1 row1 row2
                    Just ((args2, _), t2s') -> do
                        subst <- unifyAll args1 args2
                        (subst<>) <$> go (applySubst subst t1s) (applySubst subst t2s')
            go Empty t2s = remainingCont (map (\(x,(y,z)) -> (z, x, y)) t2s)


unifyAll :: (Trace, Members '[Reader LexInfo, Error TypeError, Context TypeContext, Fresh Unique] r)
          => Seq Type 
          -> Seq Type 
          -> Sem r Substitution
unifyAll Empty Empty = mempty
unifyAll (t1 :<| t1s) (t2 :<| t2s) = do
    subst <- unify t1 t2
    (subst <>) <$> unifyAll t1s t2s
unifyAll t1s t2s = error $ "unifyAll: differently sized type seqs '" <> show t1s <> "' and '" <> show t2s <> "'"

getHeadConstr :: Members '[Reader LexInfo, Error TypeError, Context TypeContext] r 
              => Type 
              -> Sem r (QualifiedName, Seq Type)
getHeadConstr (TCon qname _k) = pure (qname, [])
getHeadConstr (TApp t1 t2) = do
    (headC, args) <- getHeadConstr t1
    pure (headC, args |> t2)
getHeadConstr ty = throwType $ InvalidRowHeadConstr ty

removeRow :: HasCallStack => Type -> Type
removeRow (TRowVar [ty] _) = ty
removeRow (TRowUnif [ty] _) = ty
removeRow (TRowClosed [ty]) = ty
removeRow (TRowSkol [ty] _ _) = ty
removeRow ty = error $ "removeRow: Expected a row with a single element: " <> show ty

bind :: (Trace, Members '[Reader LexInfo, Error TypeError, Context TypeContext] r) => TVar -> Type -> Sem r Substitution
bind tv ty
    | TUnif tv == ty = pure mempty
    | occurs tv ty = throwType $ Occurs tv ty
    | TForall{} <- ty = throwType $ Impredicative tv ty
    | otherwise = do
        traceM TraceSubst $ show tv <> " := " <> show ty
        pure $ Subst (one (tv, ty)) mempty

occurs :: TVar -> Type -> Bool
occurs tv ty = tv `Set.member` freeUnifs ty


skolemize :: Members '[Fresh Unique, Output TConstraint, Reader LexInfo, Context TypeContext] r 
          => Type 
          -> Sem r (Type, Expr NextPass -> Expr NextPass)
skolemize (TForall tv ty) = do
    li <- ask
    skolem <- freshSkol tv
    (ty', f) <- skolemize (replaceTyVar tv skolem ty)
    pure (ty', TyAbs li tv . f)
skolemize (TConstraint c ty) = do
    li <- ask
    dictName <- freshVar "d"
    given c dictName
    (ty', f) <- skolemize ty
    pure (ty', DictAbs li dictName c . f)
skolemize ty = pure (ty, id)

freshSkol :: Members '[Fresh Unique] r => TVar -> Sem r Type
freshSkol tv@(MkTVar n _) = flip TSkol tv <$> freshVar (originalName n)

freshVar :: Members '[Fresh Unique] r => Text -> Sem r QualifiedName
freshVar = freshGenerated

freshTVar :: Members '[Fresh Unique] r => TVar -> Sem r TVar
freshTVar (MkTVar n k) = do
    n' <- freshVar (originalName n)
    pure (MkTVar n' k)

-- The @ignoreSubst@ method is used for type instances that don't actually do anything 
-- but are only used to allow recursion (e.g. the one for QualifiedName). 
-- The method instructs recursive instances (such as the one for Seq) to ignore
-- these completely, thereby saving potentially expensive traversals at runtime and improving asymptotics
class Substitutable a where
    applySubst :: Substitution -> a -> a
    ignoreSubst :: Bool
    ignoreSubst = False

-- The main instances
instance Substitutable Type where
    applySubst (Subst {
        substVarTys, substDicts=_substDicts
    }) = replaceUnifs substVarTys

instance Substitutable (Expr NextPass) where
    applySubst subst = \case
        -- The interesting case
        DictVarApp li e dictVar 
            | Just dict <- lookup dictVar (substDicts subst) -> DictApp li (applySubst subst e) dict
            | otherwise -> DictVarApp li (applySubst subst e) dictVar
        -- Recursive boilerplate
        App x li e1 e2 -> App (applySubst subst x) li (applySubst subst e1) (applySubst subst e2)
        IntLit x li i -> IntLit (applySubst subst x) li i
        If x li cond th el -> If (applySubst subst x) li (applySubst subst cond) (applySubst subst th) (applySubst subst el)
        Let x li decl rest -> Let (applySubst subst x) li (applySubst subst decl) (applySubst subst rest)
        Var x li var -> Var (applySubst subst x) li (applySubst subst var)
        VariantConstr x li constr -> VariantConstr (applySubst subst x) li (applySubst subst constr)
        Case x li scrut cases -> Case (applySubst subst x) li (applySubst subst scrut) (applySubst subst cases)
        Lambda x li var expr -> Lambda (applySubst subst x) li (applySubst subst var) (applySubst subst expr)
        Handle x li scrut handlers ret -> Handle (applySubst subst x) li (applySubst subst scrut) (applySubst subst handlers) (applySubst subst ret)
        Resume x li expr -> Resume (applySubst subst x) li (applySubst subst expr)
        TyApp li ty expr -> TyApp li (applySubst subst ty) (applySubst subst expr)
        TyAbs li tv expr -> TyAbs li tv (applySubst subst expr)
        DictAbs li dv constr expr -> DictAbs li (applySubst subst dv) (applySubst subst constr) (applySubst subst expr)
        DictApp li e dv -> DictApp li (applySubst subst e) (applySubst subst dv)


-- Concrete recursive instances
instance Substitutable (Statement NextPass) where
    applySubst subst = \case
        Def x li decl ty -> Def (applySubst subst x) li (applySubst subst decl) (applySubst subst ty)
        DefClass x li n tvs def -> DefClass (applySubst subst x) li (applySubst subst n) (applySubst subst tvs) (applySubst subst def)
        DefInstance x li n ty defs -> DefInstance (applySubst subst x) li (applySubst subst n) (applySubst subst ty) (applySubst subst defs)
        DefVariant x li n tvs defs -> DefVariant (applySubst subst x) li (applySubst subst n) (applySubst subst tvs) (applySubst subst defs)
        DefEffect x li n tvs defs -> DefEffect (applySubst subst x) li (applySubst subst n) (applySubst subst tvs) (applySubst subst defs)

instance Substitutable Constraint where
    applySubst subst (MkConstraint cname kind ty) = MkConstraint cname kind (applySubst subst ty)

instance Substitutable TypeContext where
    applySubst subst = \case
        WhenUnifying t1 t2 -> WhenUnifying (applySubst subst t1) (applySubst subst t2)
        WhenCheckingWanted c -> WhenCheckingWanted (applySubst subst c)
        InDefinitionFor x ty -> InDefinitionFor x (applySubst subst ty)

instance Substitutable TConstraint where
    applySubst subst (MkTConstraint constr li cxt) = MkTConstraint (applySubst subst constr) li (applySubst subst cxt)

instance Substitutable TConstraintComp where
    applySubst subst = \case
        ConUnify ty1 ty2 -> ConUnify (applySubst subst ty1) (applySubst subst ty2)
        ConWanted con dictVar
            | Just dictVar' <- lookup dictVar (substDicts subst) -> ConWanted (applySubst subst con) dictVar'
            | otherwise -> ConWanted (applySubst subst con) dictVar
        ConGiven con dictVar            
            | Just dictVar' <- lookup dictVar (substDicts subst) -> ConGiven (applySubst subst con) dictVar'
            | otherwise -> ConGiven (applySubst subst con) dictVar


instance Substitutable (Pattern NextPass) where
    applySubst subst = \case
        IntP x n -> IntP (applySubst subst x) (applySubst subst n)
        VarP x var -> VarP (applySubst subst x) (applySubst subst var)
        ConstrP x constr args -> ConstrP (applySubst subst x) (applySubst subst constr) (applySubst subst args)
        WildcardP x -> WildcardP (applySubst subst x)
        OrP x pats -> OrP (applySubst subst x) (applySubst subst pats)

instance Substitutable TypeVariant where
    applySubst subst = \case
        VariantType tvs defs -> VariantType (applySubst subst tvs) (applySubst subst defs)
        BuiltInType -> BuiltInType
        TyClass tvs defs -> TyClass (applySubst subst tvs) (applySubst subst defs)
        TyEffect tvs defs -> TyEffect (applySubst subst tvs) (applySubst subst defs)

instance Substitutable (EffHandler NextPass) where
    applySubst subst (EffHandler x li opName param expr) = EffHandler (applySubst subst x) li (applySubst subst opName) (applySubst subst param) (applySubst subst expr)

instance Substitutable (CaseBranch NextPass) where
    applySubst subst (CaseBranch x li p expr) = CaseBranch (applySubst subst x) li (applySubst subst p) (applySubst subst expr)


instance Substitutable (Decl NextPass) where
    applySubst subst (Decl x n ps e) = Decl (applySubst subst x) (applySubst subst n) (applySubst subst ps) (applySubst subst e)

-- Generic recursive instances
instance Substitutable a => Substitutable (Seq a) where
    applySubst s = if ignoreSubst @a then id else map (applySubst s)
    ignoreSubst = ignoreSubst @a

instance Substitutable a => Substitutable (Maybe a) where
    applySubst s = if ignoreSubst @a then id else fmap (applySubst s)
    ignoreSubst = ignoreSubst @a

instance (Substitutable a) => Substitutable (Map k a) where
    applySubst s = if ignoreSubst @a then id else fmap (applySubst s)
    ignoreSubst = ignoreSubst @a

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
    -- Actually branching on @ignoreSubst@ here is not going to save any performance, 
    -- since functions with @ignoreSubst@ set should not perform any work anyway
    applySubst s (x, y) = (applySubst s x, applySubst s y)
    ignoreSubst = ignoreSubst @a && ignoreSubst @b

instance (Substitutable a, Substitutable b, Substitutable c) => Substitutable (a, b, c) where
    applySubst s (x, y, z) = (applySubst s x, applySubst s y, applySubst s z)
    ignoreSubst = ignoreSubst @a && ignoreSubst @b && ignoreSubst @c

instance (Substitutable a, Substitutable b, Substitutable c, Substitutable d) => Substitutable (a, b, c, d) where
    applySubst s (x, y, z, w) = (applySubst s x, applySubst s y, applySubst s z, applySubst s w)
    ignoreSubst = ignoreSubst @a && ignoreSubst @b && ignoreSubst @c && ignoreSubst @d

-- These instances are not actually meaningful at all, but they
-- might allow other recursive instances to fire (especially the one for (,)),
-- so they are often useful in practice
instance Substitutable QualifiedName where
    applySubst _ = id
    ignoreSubst = True
instance Substitutable LexInfo where
    applySubst _ = id
    ignoreSubst = True
instance Substitutable () where
    applySubst _ = id
    ignoreSubst = True
instance Substitutable Void where
    applySubst _ = id
    ignoreSubst = True
instance Substitutable Int where
    applySubst _ = id
    ignoreSubst = True
instance Substitutable TVar where
    applySubst _ = id
    ignoreSubst = True
-- This one might change with polykinds!
instance Substitutable Kind where
    applySubst _ = id
    ignoreSubst = True
instance Substitutable Fixity where
    applySubst _ = id
    ignoreSubst = True

-- applySubst :: Data from => Substitution -> from -> from
-- applySubst s@Subst{substVarTys, substDicts} = applyDictSubst . applyTySubst
--     where
--         applyTySubst = transformBi \case
--             TUnif a' | Just t' <- lookup a' substVarTys -> t'
--             x -> x
--         applyDictSubst = transformBi \case
--             -- GHC's type checking doesn't terminate if we omit the type signature
--             ExprX (DictVarApp_ e dictVar) li :: Expr Codegen | Just dict <- lookup dictVar substDicts -> DictApp li e dict
--             x -> x



ppTC :: Seq TConstraint -> Text
ppTC = unlines . toList . map (\(MkTConstraint c l _) -> ppTConstraint c <> "    @" <> show l) 


ppTConstraint :: TConstraintComp -> Text
ppTConstraint (ConUnify t1 t2) = ppType t1 <> " ~ " <> ppType t2
ppTConstraint (ConGiven c d) = "[G] " <> show c <> " [" <> show d <> "]"
ppTConstraint (ConWanted c d) = "[W] " <> show c <> " [" <> show d <> "]"



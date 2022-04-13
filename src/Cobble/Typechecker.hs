{-#LANGUAGE TemplateHaskell#-}
module Cobble.Typechecker where

import Cobble.Prelude hiding (subsume)
import Cobble.Util
import Cobble.Util.Bitraversable
import Cobble.Util.Polysemy.Fresh
import Cobble.Util.Polysemy.Dump
import Cobble.Util.TypeUtils
import Cobble.Types
import Cobble.Types qualified as C 
import Cobble.Types.Lens
import Cobble.Core.Types qualified as Core

import qualified Data.Text as T

import qualified Data.Map as M
import Data.Traversable (for)

import qualified Data.Set as Set

import Data.List.NonEmpty qualified as NE

import Cobble.Core.Types qualified as F

type NextPass = Codegen

data TCEnv = TCEnv {
    _varTypes :: M.Map QualifiedName Type
,   _tcInstances :: M.Map QualifiedName (Seq (Type, QualifiedName))
    -- ^ Once multiparam typeclasses are implemented, this will have to be @M.Map QualfiedName (Seq (Seq Type))@
} deriving (Show, Eq, Generic, Data)

makeLenses ''TCEnv


data TypeError = DifferentTCon LexInfo QualifiedName QualifiedName
               | CannotUnify LexInfo Type Type
               | SkolBinding LexInfo Type Type
               | Occurs LexInfo TVar Type
               | Impredicative LexInfo TVar Type
               | NoInstanceFor LexInfo Constraint
               deriving (Show, Eq, Generic, Data)


data TConstraint = MkTConstraint {
    getConstraint     :: TConstraintComp 
,   constraintLexInfo :: LexInfo
} deriving (Show, Eq, Generic, Data)

data TConstraintComp = ConUnify Type Type      -- σ ~ ρ
                     | ConWanted Constraint QualifiedName
                     --                     ^ dictionary variable
                     | ConGiven Constraint QualifiedName
                     --                    ^ dictionary
                     deriving (Show, Eq, Generic, Data)

(!~) :: Members '[Output TConstraint, Reader LexInfo] r 
     => Type 
     -> Type 
     -> Sem r ()
t1 !~ t2 = ask >>= \li -> output (MkTConstraint (ConUnify t1 t2) li)
infix 1 !~

subsume :: Members '[Output TConstraint, Reader LexInfo, Fresh Text QualifiedName, Fresh TVar TVar] r 
        => Type 
        -> Type 
        -> Sem r (Expr NextPass -> Expr NextPass)
subsume t1 t2 = do
    (t1', w) <- instantiate t1
    (t2', w') <- skolemize t2
    t1' !~ t2'
    pure (w' . w) -- TODO ?

wanted :: Members '[Output TConstraint, Reader LexInfo] r 
    => Constraint
    -> QualifiedName
    -> Sem r ()
wanted c dv = ask >>= \li -> output (MkTConstraint (ConWanted c dv) li)

given :: Members '[Output TConstraint, Reader LexInfo] r 
    => Constraint
    -> QualifiedName
    -> Sem r ()
given c d = ask >>= \li -> output (MkTConstraint (ConGiven c d) li)

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
            notIdentical tv (TVar tv') | tv == tv' = False
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
insertType x t env = env & varTypes %~ insert x t

insertInstance :: QualifiedName -> Type -> QualifiedName -> TCEnv -> TCEnv
insertInstance className ty dictName = over tcInstances $ flip alter className \case
    Nothing -> Just [(ty, dictName)]
    Just is -> Just (is :|> (ty, dictName))

typecheck :: (Trace, Members '[Fresh TVar TVar, Fresh Text QualifiedName, Error TypeError, Dump (Seq TConstraint)] r)
          => TCEnv 
          -> Module Typecheck 
          -> Sem r (Module NextPass)
typecheck env (Module ext mname sts) = runReader (MkTagged mname) $ Module ext mname <$> typecheckStatements env sts

typecheckStatements :: (Trace, Members '[Fresh TVar TVar, Fresh Text QualifiedName, Error TypeError, Dump (Seq TConstraint), Reader (Tagged "ModName" Text)] r)
                    => TCEnv
                    -> (Seq (Statement Typecheck))
                    -> Sem r (Seq (Statement NextPass))
typecheckStatements env (st :<| sts) = do
    (constraints, (st', env')) <- runOutputSeq $ typecheckStatement env st
    dump constraints
    subst <- solveConstraints (_tcInstances env') mempty constraints
    
    (applySubst subst st' <|) <$> typecheckStatements env' sts
typecheckStatements env Empty = pure Empty

typecheckStatement :: (Trace, Members '[Fresh TVar TVar, Fresh Text QualifiedName, Output TConstraint, Reader (Tagged "ModName" Text)] r)
                    => TCEnv
                    -> (Statement Typecheck)
                    -> Sem r (Statement NextPass, TCEnv)
typecheckStatement env (Def fixity li decl expectedTy) = runReader li do
    (decl', env') <- checkTopLevelDecl True env decl expectedTy

    pure (Def fixity li decl' expectedTy, env')

typecheckStatement env (DefClass k li cname tvs methSigs) = do
    let env' = foldr (uncurry insertType) env methSigs
    pure (DefClass k li cname tvs methSigs, env')

typecheckStatement env (DefInstance (classKind, methDefs, params, _isImported) li cname ty meths) = runReader li do
    freshIX <- freshVar "" <&> \case
        UnsafeQualifiedName _ (LocalQName ix) -> ix
        qn -> error $ "freshVar returned non-local qname: " <> show qn

    dictName <- UnsafeQualifiedName ("d_" <> (originalName cname) <> "_" <> show freshIX) . GlobalQName <$> asks unTagged
    let env' = insertInstance cname ty dictName env

    meths' <- forM (zip meths methDefs) \(decl@(Decl _ declName _ _), (methName, methTy)) -> do
        -- sanity check
        when (methName /= declName) $ error $ "typecheckStatement: instance methods were not properly reordered at " <> show li <> "."
        
        let methTy' = case params of
                [p] -> case methTy of
                    TForall (p' :<| ps) actualMethTy
                        | p == p' -> stripConstraint (MkConstraint cname classKind ty) $ replaceTVar p ty (tforall ps actualMethTy)
                    _ -> error $ "typecheckStatement: foralls were not properly inserted in instance method type. methTy: " <> show methTy
                _ -> error "typecheckStatement: multi parameter typeclasses NYI"

        traceM DebugVerbose $ "[typecheckStatement (instance " <> show cname <> " " <> show ty <> ")]: Γ ⊢ " <> show methName <> " : " <> show methTy <> " ====> " <> show methTy'

        -- We discard the modified environment, since instance methods should really not be able to modify it.
        fst <$> checkTopLevelDecl False env' decl methTy'
    pure (DefInstance (classKind, methDefs, params, dictName) li cname ty meths', env')
        where
            stripConstraint c (TForall ps ty) = TForall ps (stripConstraint c ty)
            stripConstraint c (TConstraint c' ty) | c == c' = ty
            stripConstraint c _ = error $ "typecheckStatement: non-constrained instance method at " <> show li

typecheckStatement env (DefVariant k li tyName tvs constrs) =
    pure (DefVariant k li tyName tvs constrs', env')
        where
            resTy = foldl' (\x y -> TApp x (TVar y)) (TCon tyName k) tvs
            constrTy ps = TForall tvs (foldr (:->) resTy ps)
            constrs' = map (\(n, ps, (i, j)) -> (n, ps, (constrTy ps, i, j))) constrs
            env' = foldr (\(n,_,(t,_,_)) -> insertType n t) env constrs'
typecheckStatement env (DefEffect k li effName tvs ops) = do
    let env' = foldr (uncurry insertType) env ops
    pure (DefEffect k li effName tvs ops, env')

tforall :: Seq TVar -> Type -> Type
tforall Empty ty = ty
tforall ps ty = TForall ps ty

checkTopLevelDecl :: (Trace, Members '[Fresh TVar TVar, Fresh Text QualifiedName, Output TConstraint, Reader LexInfo] r)
      => Bool
      -> TCEnv
      -> Decl Typecheck
      -> Type
      -> Sem r (Decl NextPass, TCEnv)
checkTopLevelDecl recursive env (Decl () f xs e) expectedTy = do
    li <- ask
    (expectedTy', w) <- skolemize expectedTy
    (xs', eTy, mEff, w') <- decomposeParams expectedTy' xs
    traceM DebugVerbose $ "[checkTopLevelDecl env (" <> show f <> ")] xs' = " <> show xs' <> "mEff = " <> show mEff <> " | eTy = " <> show eTy

    let env' = if recursive
               then insertType f expectedTy env
               else env

    -- Ugh, I really don't want to have to duplicate the logic for lambdas here
    let tvs = case expectedTy of
            TForall tvs _ -> tvs
            _ -> []

    eff <- case mEff of
        Just e -> pure e
        Nothing -> freshTV KEffect

    e' <- checkPoly (foldr (\(x,ty,_) -> insertType x ty) env' xs') e eTy eff
    let lambdas = flip (foldr (TyAbs li)) tvs $ w $ makeLambdas li e' xs'

    pure (Decl (expectedTy, []) f [] lambdas, env')
        where
            makeLambdas :: LexInfo -> Expr NextPass -> Seq (QualifiedName, Type, Effect) -> Expr NextPass
            makeLambdas li = foldr (\(x, ty, eff) e -> Lambda (TFun ty eff (getType e), ty) li x e)

check :: (Trace, Members '[Output TConstraint, Fresh Text QualifiedName, Fresh TVar TVar] r)
      => TCEnv
      -> Expr Typecheck 
      -> Type 
      -> Effect
      -> Sem r (Expr NextPass)
check env e t eff | trace DebugVerbose ("[check]: Γ ⊢ " <> show e <> " : " <> ppType t <> " | " <> ppType eff) False = error "unreachable"
-- We need to 'correct' the extension field to resubstitute the skolems that were introduced by skolemize
check env e t@TForall{} eff = runReader (getLexInfo e) do
    (t', w) <- skolemize t
    w . correct t <$> check env e t' eff

check env (App () li f x) t eff = runReader li do
    (f', fValEff) <- infer env f
    (fDomTy, fFunEff, fCodomTy, w) <- decomposeFun (getType f')
    fValEff !~ fFunEff
    fValEff !~ eff
    traceM DebugVerbose $ "[check env (App ...)] getType f' = " <> ppType (getType f') <> " | t = " <> ppType t <> " | fDomTy = " <> ppType fDomTy  <> " | fFunEff = " <> ppType fFunEff <> " | fCodomTy = " <> ppType fCodomTy
    

    x' <- checkPoly env x fDomTy eff

    w' <- checkInst fCodomTy t

    pure $ w' (App t li (w f') x')

check env (IntLit () li n) t _eff = runReader li $ do
    t !~ intT 
    pure (IntLit () li n)

check env (If () li c th el) t eff = do
    c' <- check env c boolT eff
    th' <- check env th t eff
    el' <- check env el t eff
    pure (If t li c' th' el')

check env (Let () li decl@(Decl () f xs e1) e2) t eff = runReader li do
    xs' <- traverse (\x -> (x,) <$> freshTV KStar) xs

    (e1', e1Eff) <- infer (foldr (uncurry insertType) env xs') e1

    eff !~ e1Eff

    Let () li (Decl (getType e1', []) f xs' e1') <$> check (insertType f (getType e1') env) e2 t eff


check env (Var () li x) t _eff = runReader li do
    let xTy = lookupType x env
    w <- checkInst xTy t
    pure $ w (Var (t, []) li x)

check env (Ascription () li e (coercePass -> t1)) t2 eff = runReader li do
    e' <- checkPoly env e t1 eff
    w <- checkInst t1 t2
    pure $ w e'

check env (VariantConstr (i, j) li c) t _eff = runReader li do
    let cTy = lookupType c env
    w <- checkInst cTy t
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
    traceM DebugVerbose $ "[check env (Lambda ...)] t = " <> ppType t <> " | expectedArgTy = " <> ppType expectedArgTy <> " | expectedResTy = " <> ppType expectedResTy

    -- We check the expression against the effect contained in its type, *not* against
    -- the environment effect (which is irrelevant since lambdas are values)
    e' <- checkPoly (insertType x expectedArgTy env) e expectedResTy tEff 

    pure $ w (Lambda (t, expectedArgTy) li x e')


checkPattern :: Members '[Output TConstraint, Fresh Text QualifiedName, Fresh TVar TVar, Reader LexInfo] r 
             => TCEnv
             -> Pattern Typecheck
             -> Type
             -> Sem r (Pattern NextPass, TCEnv -> TCEnv)
checkPattern env pats ty = evalState mempty $ go Nothing env pats ty
    where
        go :: Members '[Output TConstraint, Fresh Text QualifiedName, Fresh TVar TVar, Reader LexInfo, State (Map QualifiedName Type)] r
           => Maybe (Map QualifiedName Type)
           -> TCEnv
           -> Pattern Typecheck
           -> Type
           -> Sem r (Pattern NextPass, TCEnv -> TCEnv)
        go _ env (IntP () n) t = pure (IntP () n, id)
        
        go Nothing env (VarP () x) t = do 
            modify (insert x t)
            pure (VarP t x, insertType x t)
        
        go (Just orPatTys) env (VarP () x) t = case lookup x orPatTys of
            Nothing -> error $ "checkPattern: variable '" <> show x <> "' not bound by or pattern. orPatTys=" <> show orPatTys
            Just ty -> do
                t !~ ty -- TODO: Use subsumption?
                pure (VarP t x, id)

        go mOrPatTys env (ConstrP (i,v) cname ps) t = do
            (constrTy, w) <- instantiate (lookupType cname env)
            (typedPats, resTy, _meff, w') <- decomposeParams constrTy ps
            -- TODO: I don't know...
            resTy !~ t
            (ps', exts) <- unzip <$> forM typedPats \(p, pTy, _eff) -> go mOrPatTys env p pTy
            pure (ConstrP (t,i,v) cname ps', foldr (.) id exts)
        go _ env (WildcardP _) t = pure (WildcardP t, id)
        go mOrPatTys env (OrP () (p :<| pats)) t = do
            (boundTys, (p', pWrapper)) <- runState mempty $ go mOrPatTys env p t
            
            (pats', wrappers) <- unzip <$> traverse (flip (go (mOrPatTys <> Just boundTys) env) t) pats

            pure (OrP (getType p') (p' :<| pats'), foldr (.) pWrapper wrappers)
        go _ _ (OrP () Empty) _ = error "checkPattern: Empty Or-Pattern"

infer :: (Trace, Members '[Output TConstraint, Fresh Text QualifiedName, Fresh TVar TVar] r )
      => TCEnv
      -> Expr Typecheck 
      -> Sem r (Expr NextPass, Effect)
infer env (App () li f x) = runReader li do
    (f', fValEff) <- infer env f
    (fDomTy, fFunEff, fCodomTy, wF) <- decomposeFun (getType f') 
    traceM DebugVerbose $ "[infer env (FCall ...)] getType f' = " <> ppType (getType f') <> " | fDomTy = " <> ppType fDomTy <> " | fFunEff = " <> ppType fFunEff <> " | fCodomTy = " <> ppType fCodomTy
    fValEff !~ fFunEff

    x' <- checkPoly env x fDomTy fFunEff

    (t, w) <- instantiate fCodomTy

    pure (w (App t li (wF f') x'), fFunEff)
infer env (IntLit () li n) = do
    eff <- freshTV KEffect
    pure (IntLit () li n, eff)
infer env (If () li c th el) = runReader li do
    -- This is hard, since we have to make sure th and el have the same type,
    -- but we cannot simply use (!~), which could not infer higher rank types.
    -- Instead, we check that both types are equivalent using subsumption:
    --     getType th' <= getType el' /\ getType el' <= getType th'.
    (c', cEff)  <- infer env c
    (th', thEff) <- infer env th
    (el', elEff) <- infer env el
    
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

infer env (Let () li decl@(Decl () f xs e1) e2) = runReader li do
    xs' <- traverse (\x -> (x,) <$> freshTV KStar) xs

    (e1', e1Eff) <- infer (foldr (uncurry insertType) env xs') e1
    (e2', e2Eff) <- infer (insertType f (getType e1') env) e2

    e1Eff !~ e2Eff

    pure (Let () li (Decl (getType e1', []) f xs' e1') e2', e1Eff)

infer env (Var () li x) = runReader li do
    let xTy = lookupType x env
    (ty, w) <- instantiate xTy

    eff <- freshTV KEffect
    pure (w (Var (ty, []) li x), eff)

infer env (Ascription () li e t) = runReader li do
    -- TODO: Can we really just check against a fresh effect variable?
    eff <- freshTV KEffect
    e' <- checkPoly env e t eff
    (t2, w) <- instantiate t
    pure (w e', eff)


infer env (VariantConstr (i, j) li c) = runReader li do
    let cTy = lookupType c env
    (cTy', w) <- instantiate cTy
    
    eff <- freshTV KEffect
    pure (w $ VariantConstr (cTy', cTy, j) li c, eff)

infer env (Case () li e branches) = runReader li do
    (e', eEff) <- infer env e
    branches' <- forM branches \(CaseBranch () li p brExpr) -> do
        (p', extendEnv) <- checkPattern env p (getType e')
        (brExpr', brEff) <- infer (extendEnv env) brExpr
        pure (CaseBranch () li p' brExpr', brEff)
    

    -- We have to make sure all branches have the same return type.
    -- See 'infer env (If ...)' for a more detailed explanation
    (t, eff) <- checkEquiv (map (\(CaseBranch _ _ _ expr, eff) -> (getType expr, eff)) branches')
    pure (Case t li e' (map fst branches'), eff)
    where
        checkEquiv :: forall r. Members '[Reader LexInfo, Output TConstraint, Fresh Text QualifiedName, Fresh TVar TVar] r 
                   => Seq (Type, Effect) 
                   -> Sem r (Type, Effect)
        checkEquiv Empty                = (,) <$> freshTV KStar <*> freshTV KEffect -- the case expression is empty.
        checkEquiv ((t, eff) :<| Empty) = pure (t, eff)
        checkEquiv ((t1, eff1):<|(t2, eff2):<|ts) = do
            w1 <- subsume t1 t2 -- TODO: apply wrapping?
            w2 <- subsume t2 t1
            eff1 !~ eff2
            checkEquiv ((t2, eff2)<|ts)


infer env (Lambda () li x e) = do
    xTy <- freshTV KStar

    (e', eEff) <- infer (insertType x xTy env) e

    -- The lambda captures the body's effect in the function type, but it itself has a fully polymorphic
    -- effect type.
    lamEff <- freshTV KEffect 
    pure (Lambda (TFun xTy eEff (getType e'), xTy) li x e', lamEff)

checkPoly :: (Trace, Members '[Output TConstraint, Fresh Text QualifiedName, Fresh TVar TVar, Reader LexInfo] r)
          => TCEnv
          -> Expr Typecheck
          -> Type 
          -> Effect
          -> Sem r (Expr NextPass)
checkPoly _ _ ty eff | trace DebugVerbose ("checkPoly " <> ppType ty) False = error "unreachable"
checkPoly env expr (TForall tvs ty) eff = ask >>= \li -> do
    flip (foldr (TyAbs li)) tvs <$> checkPoly env expr ty eff
checkPoly env expr ty eff = check env expr ty eff

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


checkInst :: Members '[Output TConstraint, Reader LexInfo, Fresh Text QualifiedName, Fresh TVar TVar] r 
          => Type 
          -> Type 
          -> Sem r (Expr NextPass -> Expr NextPass)
checkInst = subsume

instantiate :: Members '[Fresh Text QualifiedName, Fresh TVar TVar, Reader LexInfo, Output TConstraint] r 
            => Type 
            -> Sem r (Type, Expr NextPass -> Expr NextPass)
instantiate (TForall tvs ty) = ask >>= \li -> do
    -- The substitution is not immediately converted to a Map, since the order
    -- of tyvars is important
    tvSubst <- traverse (\tv -> (tv,) . TVar <$> freshVar tv) tvs
    
    (ty', w) <- instantiate (replaceTVars (M.fromList (toList tvSubst)) ty)

    pure (ty', foldr (\(_,x) r -> r . TyApp li x) w tvSubst)
instantiate (TConstraint c ty) = do
    li <- ask
    dictVar <- freshVar "dv"
    wanted c dictVar
    (ty', w) <- instantiate ty
    pure (ty', \e -> w $ DictVarApp li e dictVar)
instantiate ty = do
    pure (ty, id)


decomposeFun :: Members '[Fresh Text QualifiedName, Fresh TVar TVar, Reader LexInfo, Output TConstraint] r 
          => Type 
          -> Sem r (Type, Effect, Type, Expr NextPass -> Expr NextPass)
decomposeFun t@TForall{} = do
    (t', w) <- instantiate t
    (\(x, eff, y, w') -> (x, eff, y, w . w')) <$> decomposeFun t'

decomposeFun (TFun a eff b) = pure (a, eff, b, id)
decomposeFun t = do
    argTy <- freshTV KStar
    effTy <- freshTV KEffect
    resTy <- freshTV KStar
    w <- subsume t (TFun argTy effTy resTy)
    pure (argTy, effTy, resTy, w)

decomposeParams :: Members '[Fresh Text QualifiedName, Fresh TVar TVar, Reader LexInfo, Output TConstraint] r
                => Type
                -> Seq a
                -> Sem r (Seq (a, Type, Effect), Type, Maybe Effect, Expr NextPass -> Expr NextPass)
decomposeParams ty Empty = pure ([], ty, Nothing, id)
decomposeParams ty (x :<| xs) = do
    (argTy, eff, resTy, w) <- decomposeFun ty 
    (restArgTys, restResTy, mEff, w') <- decomposeParams resTy xs
    pure ((x,argTy, eff) <| restArgTys, restResTy, mEff <|> Just eff, w . w')


lambdasToDecl :: LexInfo -> QualifiedName -> Expr NextPass -> Int -> Decl NextPass
lambdasToDecl li f = go []
    where
        go :: Seq (QualifiedName, Type) -> Expr NextPass -> Int -> Decl NextPass
        go args e 0                      = Decl (getType e, []) f (reverse args) e
        go args (Lambda (_, t) _ x e) n  = go ((x,t)<|args) e (n - 1)
        go args e n                      = error $ "Typechecker.lambdasToDecl: suppplied lambda did not have enough parameters.\n  Remaining parameters: " <> show n <> "\n  Expression: " <> show e


freshTV :: Members '[Fresh Text QualifiedName] r => Kind -> Sem r Type
freshTV k = freshVar "u" <&> \u -> TVar (MkTVar u k) 

replaceTVar :: TVar -> Type -> Type -> Type
replaceTVar tv ty = replaceTVars (one (tv, ty))

replaceTVars :: Map TVar Type -> Type -> Type
replaceTVars tvs ty@(TVar tv) = case lookup tv tvs of
                Just ty' -> ty'
                Nothing -> ty
replaceTVars tvs ty@TCon{} = ty
replaceTVars tvs ty@TSkol{} = ty
replaceTVars tvs ty@(TApp t1 t2) = TApp (replaceTVars tvs t1) (replaceTVars tvs t2)
replaceTVars tvs ty@(TFun a eff b) = TFun (replaceTVars tvs a) (replaceTVars tvs eff) (replaceTVars tvs b)
replaceTVars tvs (TForall forallTVs ty) =
                let remainingTVs = foldr (M.delete) tvs forallTVs in
                TForall forallTVs $ replaceTVars remainingTVs ty
replaceTVars tvs (TConstraint (MkConstraint constrName k constrTy) ty) =
                TConstraint (MkConstraint constrName k (replaceTVars tvs constrTy)) (replaceTVars tvs ty)
replaceTVars tvs (TRowClosed tys)   = TRowClosed (map (replaceTVars tvs) tys)
replaceTVars tvs (TRowOpen tys var) = let replacedTys = map (replaceTVars tvs) tys in case lookup var tvs of
    Nothing                         -> TRowOpen replacedTys var
    Just (TVar var')                -> TRowOpen replacedTys var'
    Just (TSkol skol var')          -> TRowSkol replacedTys skol var'
    Just (TRowClosed tys')          -> TRowClosed (replacedTys <> tys')
    Just (TRowOpen tys' var')       -> TRowOpen (replacedTys <> tys') var'
    Just (TRowSkol tys' skol var')  -> TRowSkol (replacedTys <> tys') skol var'
    Just ty -> error $ "replaceTVars: Trying to replace variable '" <> show var <> "' in open row type '" <> show (TRowOpen tys var)
                        <> "' with non-row type: " <> show ty
replaceTVars tvs (TRowSkol tys skol var) = TRowSkol (map (replaceTVars tvs) tys) skol var

solveConstraints :: (Trace, Members '[Error TypeError, Fresh Text QualifiedName, Fresh TVar TVar] r)
                 => Map QualifiedName (Seq (Type, QualifiedName))
                 --                              v^dict
                 -> Seq (Constraint, QualifiedName, LexInfo)
                 -> Seq TConstraint
                 -> Sem r Substitution
solveConstraints givens wanteds Empty = solveWanteds givens wanteds
solveConstraints givens wanteds ((MkTConstraint (ConUnify t1 t2) li) :<| constrs) = runReader li do
            -- This is a bit inefficient (quadratic?).
            -- Let's make sure the constraint solver actually works, before we try to deal with that.
            subst <- unify t1 t2
            (subst <>) <$> solveConstraints (applySubst subst givens) (applySubst subst wanteds) (applySubst subst constrs)
solveConstraints givens wanteds ((MkTConstraint (ConWanted c dictVar) li) :<| constrs) = runReader li do
            solveConstraints givens ((c, dictVar, li) :<| wanteds) constrs
        
solveConstraints givens wanteds ((MkTConstraint (ConGiven c@(MkConstraint cname k ty) dict) li) :<| constrs) = runReader li do
            let givens' = alter (<> Just [(ty, dict)]) cname givens
            solveConstraints givens' wanteds constrs

solveWanteds :: (Trace, Members '[Error TypeError] r)
                => Map QualifiedName (Seq (Type, QualifiedName))
                 --                              v^dict
                -> Seq (Constraint, QualifiedName, LexInfo)
                -> Sem r Substitution
solveWanteds givens Empty = pure mempty
solveWanteds givens ((c@(MkConstraint cname k ty), dictVar, li) :<| wanteds) = runReader li do
    case lookup cname givens of
        Just tys -> do
            let trySolve Empty = throw $ NoInstanceFor li c
                trySolve ((t2, d) :<| tys) = do
                    runError @TypeError (unify ty t2) >>= \case
                        Left err -> do
                            traceM DebugVerbose $ "[solveWanteds] [W] " <> show c <> ": " <> show ty <> " ~ " <> show t2 <> ": " <> show err
                            trySolve tys
                        Right subst -> do
                            traceM DebugVerbose $ "[solveWanteds] [W] " <> show c <> ": " <> show ty <> " ~ " <> show t2 <> " ✓"
                            pure $ subst <> Subst mempty (one (dictVar, d)) -- Substitute the dictionary
            -- We can rely on coherence and non-overlapping instances here and just pick the
            -- first matching dictionary that we find.
            -- TODO: Should we throw an ambiguity error if subst contains non-empty tyvar substitutions?
            subst <- trySolve tys
            (subst <>) <$> solveWanteds (applySubst subst givens) (applySubst subst wanteds)
        Nothing -> throw $ NoInstanceFor li c

-- TODO: Unify rows
unify :: Members '[Error TypeError, Reader LexInfo] r
      => Type
      -> Type
      -> Sem r Substitution
unify t1@(TCon c1 _) t2@(TCon c2 _)
    | c1 == c2 = pure mempty
    | otherwise = ask >>= \li -> throw $ DifferentTCon li c1 c2
unify (TVar tv) t2 = bind tv t2
unify t1 (TVar tv) = bind tv t1
unify (TApp a1 b1) (TApp a2 b2) = do
    subst <- unify a1 a2
    (subst <>) <$> unify (applySubst subst b1) (applySubst subst b2)
unify (TFun a1 eff1 b1) (TFun a2 eff2 b2) = do
    subst <- unify a1 a2
    subst' <- (subst <>) <$> unify (applySubst subst eff1) (applySubst subst eff2)
    (subst' <>) <$> unify (applySubst subst' b1) (applySubst subst' b2)
unify t1@TSkol{} t2@TSkol{}
    | t1 == t2 = pure mempty


unify s@TSkol{} t2  = ask >>= \li -> throw $ SkolBinding li s t2
unify t1 s@TSkol{}  = ask >>= \li -> throw $ SkolBinding li t1 s
unify t1 t2         = ask >>= \li -> throw $ CannotUnify li t1 t2 

bind :: Members '[Reader LexInfo, Error TypeError] r => TVar -> Type -> Sem r Substitution
bind tv ty
    | TVar tv == ty = pure mempty
    | occurs tv ty = ask >>= \li -> throw (Occurs li tv ty)
    | TForall{} <- ty = ask >>= \li -> throw (Impredicative li tv ty)
    | otherwise = pure $ Subst (one (tv, ty)) mempty

occurs :: TVar -> Type -> Bool
occurs tv ty = tv `Set.member` freeTVs ty


skolemize :: Members '[Fresh Text QualifiedName, Output TConstraint, Reader LexInfo] r 
          => Type 
          -> Sem r (Type, Expr NextPass -> Expr NextPass)
skolemize (TForall tvs ty) = do
    skolemMap <- M.fromList . toList <$> traverse (\tv@(MkTVar n _) -> (tv,) . flip TSkol tv <$> freshVar (originalName n)) tvs
    skolemize $ replaceTVars skolemMap ty
skolemize (TConstraint c ty) = do
    li <- ask
    dictName <- freshVar "d"
    given c dictName
    (ty', f) <- skolemize ty
    pure (ty', f . DictAbs li dictName c)
skolemize ty = pure (ty, id)

applySubst :: Data from => Substitution -> from -> from
applySubst s@Subst{substVarTys, substDicts} = applyDictSubst . applyTySubst
    where
        applyTySubst = transformBi \case
            TVar a' | Just t' <- lookup a' substVarTys -> t'
            x -> x
        applyDictSubst = transformBi \case
            -- GHC's type checking doesn't terminate if we omit the type signature
            ExprX (DictVarApp_ e dictVar) li :: Expr Codegen | Just dict <- lookup dictVar substDicts -> DictApp li e dict
            x -> x



ppTC :: Seq TConstraint -> Text
ppTC = unlines . toList . map (\(MkTConstraint c l) -> ppTConstraint c <> "    @" <> show l) 

ppWanteds :: Seq TWanted -> Text
ppWanteds = unlines . toList . map (\(TWanted c li) -> ppConstraint c <> " @" <> show li)

ppGivens :: Seq TGiven -> Text
ppGivens = unlines . toList . map (\(TGiven c li) -> ppConstraint c <> " @" <> show li)


ppTConstraint :: TConstraintComp -> Text
ppTConstraint (ConUnify t1 t2) = ppType t1 <> " ~ " <> ppType t2
ppTConstraint (ConGiven c d) = "[G] " <> show c <> " [" <> show d <> "]"
ppTConstraint (ConWanted c d) = "[W] " <> show c <> " [" <> show d <> "]"



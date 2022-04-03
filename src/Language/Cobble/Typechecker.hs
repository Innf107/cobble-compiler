{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Typechecker where

import Language.Cobble.Prelude hiding (subsume)
import Language.Cobble.Util
import Language.Cobble.Util.Convert
import Language.Cobble.Util.Bitraversable
import Language.Cobble.Util.Polysemy.Fresh
import Language.Cobble.Util.Polysemy.Dump
import Language.Cobble.Types
import Language.Cobble.Types qualified as C 
import Language.Cobble.Types.Lens
import Language.Cobble.Core.Types qualified as Core

import qualified Data.Text as T

import qualified Data.Map as M
import Data.Traversable (for)

import qualified Data.Set as Set

import Data.List.NonEmpty qualified as NE

import Language.Cobble.Core.Types qualified as F

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

typecheck :: (Trace, Members '[Fresh TVar TVar, Fresh Text QualifiedName, Error TypeError, Dump (Seq TConstraint)] r)
          => TCEnv 
          -> Module Typecheck 
          -> Sem r (Module NextPass)
typecheck env (Module ext mname sts) = Module ext mname <$> typecheckStatements env sts

typecheckStatements :: (Trace, Members '[Fresh TVar TVar, Fresh Text QualifiedName, Error TypeError] r)
                    => TCEnv
                    -> (Seq (Statement Typecheck))
                    -> Sem r (Seq (Statement NextPass))
typecheckStatements env (st :<| sts) = do
    (constraints, (st', env')) <- runOutputSeq $ typecheckStatement env st
    subst <- solveConstraints (_tcInstances env) constraints
    
    (applySubst subst st' <|) <$> typecheckStatements env' sts
typecheckStatements env Empty = pure Empty

typecheckStatement :: (Trace, Members '[Fresh TVar TVar, Fresh Text QualifiedName, Output TConstraint] r)
                    => TCEnv
                    -> (Statement Typecheck)
                    -> Sem r (Statement NextPass, TCEnv)
typecheckStatement env (Def fixity li (Decl () f xs e) expectedTy) = runReader li do
    (expectedTy', w) <- skolemize expectedTy
    (xs', eTy, w') <- decomposeParams expectedTy' xs
    traceM DebugVerbose $ "[typecheckStatements env (Def ...)] xs' = " <> show xs' <> " | eTy = " <> show eTy

    let env' = insertType f expectedTy env

    -- Ugh, I really don't want to have to duplicate the logic for lambdas here
    let tvs = case expectedTy of
            TForall tvs _ -> tvs
            _ -> []

    e' <- checkPoly (foldr (uncurry insertType) env' xs') e eTy
    let lambdas = flip (foldr (TyAbs li)) tvs $ w $ makeLambdas e' xs'

    pure (Def fixity li (Decl (expectedTy, []) f [] lambdas) expectedTy, env')
    where
        makeLambdas :: Expr NextPass -> Seq (QualifiedName, Type) -> Expr NextPass
        makeLambdas = foldr (\(x, ty) e -> Lambda (ty :-> getType e, ty) li x e)

typecheckStatement env (Import () li name) = pure (Import () li name, env)
typecheckStatement env (DefClass k li cname tvs methSigs) = do
    let env' = foldr (uncurry insertType) env methSigs
    pure (DefClass k li cname tvs methSigs, env')

typecheckStatement env (DefInstance x li cname ty meths) = undefined
typecheckStatement env (DefVariant k li tyName tvs constrs) =
    pure (DefVariant k li tyName tvs constrs', env')
        where
            resTy = foldl' (\x y -> TApp x (TVar y)) (TCon tyName k) tvs
            constrTy ps = TForall tvs (foldr (:->) resTy ps)
            constrs' = map (\(n, ps, (i, j)) -> (n, ps, (constrTy ps, i, j))) constrs
            env' = foldr (\(n,_,(t,_,_)) -> insertType n t) env constrs'

check :: (Trace, Members '[Output TConstraint, Fresh Text QualifiedName, Fresh TVar TVar] r)
      => TCEnv
      -> Expr Typecheck 
      -> Type 
      -> Sem r (Expr NextPass)
check env e t | trace DebugVerbose ("[check]: Γ ⊢ " <> show e <> " : " <> ppType t) False = error "unreachable"
-- We need to 'correct' the extension field to resubstitute the skolems that were introduced by skolemize
check env e t@TForall{} = runReader (getLexInfo e) do
    (t', w) <- skolemize t
    w . correct t <$> check env e t'
check env (App () li f x) t = runReader li do
    f' <- infer env f
    (fDomTy, fCodomTy, w) <- decomposeFun (getType f')
    traceM DebugVerbose $ "[check env (FCall ...)] getType f' = " <> ppType (getType f') <> " | t = " <> ppType t <> " | fDomTy = " <> ppType fDomTy <> " | fCodomTy = " <> ppType fCodomTy
    
    x' <- checkPoly env x fDomTy

    w' <- checkInst fCodomTy t

    pure $ w' (App t li (w f') x')

check env (IntLit () li n) t = runReader li $ do
    t !~ intT 
    pure (IntLit () li n)
check env (UnitLit li) t = runReader li do
    t !~ unitT
    pure (UnitLit li)
check env (If () li c th el) t = do
    c' <- check env c boolT
    th' <- check env th t
    el' <- check env el t
    pure (If t li c' th' el')
check env (Let () li decl@(Decl () f xs e1) e2) t = runReader li do
    xs' <- traverse (\x -> (x,) <$> freshTV KStar) xs

    e1' <- infer (foldr (uncurry insertType) env xs') e1

    Let () li (Decl (getType e1', []) f xs' e1') <$> check (insertType f (getType e1') env) e2 t


check env (Var () li x) t = runReader li do
    let xTy = lookupType x env
    w <- checkInst xTy t
    pure $ w (Var (t, []) li x)

check env (Ascription () li e (coercePass -> t1)) t2 = runReader li do
    e' <- checkPoly env e t1
    w <- checkInst t1 t2
    pure $ w e'

check env (VariantConstr (i, j) li c) t = runReader li do
    let cTy = lookupType c env
    w <- checkInst cTy t
    pure $ w (VariantConstr (t, cTy, j) li c)

check env (Case () li e branches) t = do
    e' <- infer env e
    branches' <- forM branches \(CaseBranch () li p brExpr) -> runReader li do
        (p', extendEnv) <- checkPattern env p (getType e')
        brExpr' <- check (extendEnv env) brExpr t
        pure (CaseBranch () li p' brExpr')
    pure $ Case t li e' branches'
    
check env (Lambda () li x e) t = runReader li do
    (expectedArgTy, expectedResTy, w) <- decomposeFun t
    traceM DebugVerbose $ "[check env (Lambda ...)] t = " <> ppType t <> " | expectedArgTy = " <> ppType expectedArgTy <> " | expectedResTy = " <> ppType expectedResTy

    e' <- checkPoly (insertType x expectedArgTy env) e expectedResTy

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
            (typedPats, resTy, w') <- decomposeParams constrTy ps
            -- TODO: I don't know...
            resTy !~ t
            (ps', exts) <- unzip <$> forM typedPats \(p, pTy) -> go mOrPatTys env p pTy
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
      -> Sem r (Expr NextPass)
infer env (App () li f x) = runReader li do
    f' <- infer env f
    (fDomTy, fCodomTy, wF) <- decomposeFun (getType f') 
    traceM DebugVerbose $ "[infer env (FCall ...)] getType f' = " <> ppType (getType f') <> " | fDomTy = " <> ppType fDomTy <> " | fCodomTy = " <> ppType fCodomTy
    
    x' <- checkPoly env x fDomTy

    (t, w) <- instantiate fCodomTy

    pure (w (App t li (wF f') x'))

infer env (IntLit () li n) = pure (IntLit () li n)
infer env (UnitLit li) = pure (UnitLit li)
infer env (If () li c th el) = runReader li do
    -- This is hard, since we have to make sure th and el have the same type,
    -- but we cannot simply use (!~), which could not infer higher rank types.
    -- Instead, we check that both types are equivalent using subsumption:
    --     getType th' <= getType el' /\ getType el' <= getType th'.
    c'  <- infer env c
    th' <- infer env th
    el' <- infer env el
    
    getType c' !~ boolT

    -- Make sure that the types of th' and el' are
    -- equivalent (though they may not necessarily *look* the same).
    w1 <- subsume (getType th') (getType el')
    w2 <- subsume (getType el') (getType th')

    -- We arbitrarily choose getType th' as the final type, since the types of
    -- th' and el' are equivalent.
    pure $ If (getType th') li c' (w1 th') (w2 el')
infer env (Let () li decl@(Decl () f xs e1) e2) = do
    xs' <- traverse (\x -> (x,) <$> freshTV KStar) xs

    e1' <- infer (foldr (uncurry insertType) env xs') e1

    Let () li (Decl (getType e1', []) f xs' e1') <$> infer (insertType f (getType e1') env) e2
    
infer env (Var () li x) = runReader li do
    let xTy = lookupType x env
    (ty, w) <- instantiate xTy
    pure $ w (Var (ty, []) li x)

infer env (Ascription () li e (coercePass -> t)) = runReader li do
    e' <- checkPoly env e t
    (t2, w) <- instantiate t
    pure (w e')

infer env (VariantConstr (i, j) li c) = runReader li do
    let cTy = lookupType c env
    (cTy', w) <- instantiate cTy
    pure (w $ VariantConstr (cTy', cTy, j) li c)

infer env (Case () li e branches) = runReader li do
    e' <- infer env e
    branches' <- forM branches \(CaseBranch () li p brExpr) -> do
        (p', extendEnv) <- checkPattern env p (getType e')
        brExpr' <- infer (extendEnv env) brExpr
        pure (CaseBranch () li p' brExpr')
    

    -- We have to make sure all branches have the same return type.
    -- See 'infer env (If ...)' for a more detailed explanation
    t <- checkEquiv (map (\(CaseBranch _ _ _ expr) -> getType expr) branches')
    pure (Case t li e' branches')
    where
        checkEquiv :: forall r. Members '[Reader LexInfo, Output TConstraint, Fresh Text QualifiedName, Fresh TVar TVar] r 
                   => Seq Type 
                   -> Sem r Type
        checkEquiv Empty           = freshTV KStar -- the case expression is empty
        checkEquiv (t :<| Empty)  = pure t
        checkEquiv (t1:<|t2:<|ts) = do
            w1 <- subsume t1 t2 -- TODO: apply wrapping?
            w2 <- subsume t2 t1
            checkEquiv (t2<|ts)


infer env (Lambda () li x e) = do
    xTy <- freshTV KStar

    e' <- infer (insertType x xTy env) e

    pure (Lambda (xTy :-> getType e', xTy) li x e')

checkPoly :: (Trace, Members '[Output TConstraint, Fresh Text QualifiedName, Fresh TVar TVar, Reader LexInfo] r)
          => TCEnv
          -> Expr Typecheck 
          -> Type 
          -> Sem r (Expr NextPass)
checkPoly _ _ ty | trace DebugVerbose ("checkPoly " <> ppType ty) False = error "unreachable"
checkPoly env expr (TForall tvs ty) = ask >>= \li -> do
    flip (foldr (TyAbs li)) tvs <$> checkPoly env expr ty
checkPoly env expr ty = check env expr ty

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
 -- TODO: Insert Dictionary application?
    (ty', w) <- instantiate ty
    pure (ty', \e -> w $ DictVarApp li e dictVar)
instantiate ty = do
    pure (ty, id)


decomposeFun :: Members '[Fresh Text QualifiedName, Fresh TVar TVar, Reader LexInfo, Output TConstraint] r 
          => Type 
          -> Sem r (Type, Type, Expr NextPass -> Expr NextPass)
decomposeFun t@TForall{} = do
    (t', w) <- instantiate t
    (\(x, y, w') -> (x, y, w . w')) <$> decomposeFun t'
decomposeFun (TFun a b) = pure (a, b, id)
decomposeFun t = do
    argTy <- freshTV KStar
    resTy <- freshTV KStar
    w <- subsume t (argTy :-> resTy)
    pure (argTy, resTy, w)

decomposeParams :: Members '[Fresh Text QualifiedName, Fresh TVar TVar, Reader LexInfo, Output TConstraint] r
                => Type
                -> Seq a
                -> Sem r (Seq (a, Type), Type, Expr NextPass -> Expr NextPass)
decomposeParams ty Empty = pure ([], ty, id)
decomposeParams ty (x:<|xs) = do
    (argTy, resTy, w) <- decomposeFun ty 
    (restArgTys, restResTy, w') <- decomposeParams resTy xs
    pure ((x,argTy) <| restArgTys, restResTy, w . w')


lambdasToDecl :: LexInfo -> QualifiedName -> Expr NextPass -> Int -> Decl NextPass
lambdasToDecl li f = go []
    where
        go :: Seq (QualifiedName, Type) -> Expr NextPass -> Int -> Decl NextPass
        go args e 0                      = Decl (getType e, []) f (reverse args) e
        go args (Lambda (_, t) _ x e) n  = go ((x,t)<|args) e (n - 1)
        go args e n                      = error $ "Typechecker.lambdasToDecl: suppplied lambda did not have enough parameters.\n  Remaining parameters: " <> show n <> "\n  Expression: " <> show e


freshTV :: Members '[Fresh Text QualifiedName] r => Kind -> Sem r Type
freshTV k = freshVar "u" <&> \u -> TVar (MkTVar u k) 

replaceTVars :: Map TVar Type -> Type -> Type
replaceTVars tvs ty@(TVar tv) = case lookup tv tvs of
                Just ty' -> ty'
                Nothing -> ty
replaceTVars tvs ty@TCon{} = ty
replaceTVars tvs ty@TSkol{} = ty
replaceTVars tvs ty@(TApp t1 t2) = TApp (replaceTVars tvs t1) (replaceTVars tvs t2)
replaceTVars tvs ty@(TFun a b)   = TFun (replaceTVars tvs a) (replaceTVars tvs b)
replaceTVars tvs (TForall forallTVs ty) =
                let remainingTVs = foldr (M.delete) tvs forallTVs in
                TForall forallTVs $ replaceTVars remainingTVs ty
replaceTVars tvs (TConstraint (MkConstraint constrName constrTy) ty) =
                TConstraint (MkConstraint constrName (replaceTVars tvs constrTy)) (replaceTVars tvs ty)


solveConstraints :: Members '[Error TypeError, Fresh Text QualifiedName, Fresh TVar TVar] r
                 => Map QualifiedName (Seq (Type, QualifiedName))
                 --                               ^dict
                 -> Seq TConstraint
                 -> Sem r Substitution
solveConstraints _ Empty = pure mempty
solveConstraints givens ((MkTConstraint (ConUnify t1 t2) li) :<| constrs) = runReader li do
            -- This is a bit inefficient (quadratic?).
            -- Let's make sure the constraint solver actually works, before we try to deal with that.
            subst <- unify t1 t2
            (subst <>) <$> solveConstraints givens (applySubst subst constrs)
solveConstraints givens ((MkTConstraint (ConWanted c@(MkConstraint cname ty) dictVar) li) :<| constrs) = runReader li do
            case lookup cname givens of
                Just tys -> do
                    let trySolve Empty = pure Nothing
                        trySolve ((t2, d) :<| tys) = do
                            runError (unify ty t2) >>= \case
                                Left _ -> trySolve tys
                                Right subst -> pure $ Just $ subst <> Subst mempty (one (dictVar, d)) -- Substitute the dictionary
                    -- We can rely on coherence and non-overlapping instances here and just pick the
                    -- first matching dictionary that we find.
                    trySolve tys >>= \case
                        Nothing  -> throw $ NoInstanceFor li c
                        Just subst -> do
                            -- TODO: Apply dictionary somehow
                            (subst <>) <$> solveConstraints givens (applySubst subst constrs)
                Nothing -> throw $ NoInstanceFor li c

solveConstraints givens ((MkTConstraint (ConGiven c@(MkConstraint cname ty) dict) li) :<| constrs) = runReader li do
            let givens' = alter (<> Just [(ty, dict)]) cname givens
            solveConstraints givens' constrs

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
unify (TFun a1 b1) (TFun a2 b2) = do
    subst <- unify a1 a2
    (subst <>) <$> unify (applySubst subst b1) (applySubst subst b2)
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



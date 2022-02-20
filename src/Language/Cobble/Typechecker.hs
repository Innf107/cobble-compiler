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

type NextPass = Codegen

data TCEnv = TCEnv {
    _varTypes :: M.Map QualifiedName Type
,   _tcInstances :: M.Map QualifiedName [Type] 
    -- ^ Once multiparam typeclasses are implemented, this will have to be @M.Map QualfiedName [[Type]]@
} deriving (Show, Eq, Generic, Data)

makeLenses ''TCEnv


data TypeError = DifferentTCon LexInfo QualifiedName QualifiedName
               | CannotUnify LexInfo Type Type
               | SkolBinding LexInfo TVar Type
               | Occurs LexInfo TVar Type
               | Impredicative LexInfo TVar Type
               deriving (Show, Eq, Generic, Data)


data TConstraint = MkTConstraint {
    getConstraint     :: TConstraintComp 
,   constraintLexInfo :: LexInfo
} deriving (Show, Eq, Generic, Data)

data TConstraintComp = Unify Type Type      -- σ ~ ρ
                     | Subsume Type Type    -- σ ≤ ρ
                     deriving (Show, Eq, Generic, Data)

(!~) :: Members '[Output TConstraint, Reader LexInfo] r => Type -> Type -> Sem r ()
t1 !~ t2 = ask >>= \li -> output (MkTConstraint (Unify t1 t2) li)
infix 1 !~

subsume :: Members '[Output TConstraint, Reader LexInfo] r => Type -> Type -> Sem r ()
subsume t1 t2 = ask >>= \li -> output (MkTConstraint (Subsume t1 t2) li)


newtype Substitution = Subst {unSubst :: Map TVar Type} 
    deriving stock   (Show, Eq, Generic, Data)

-- Not sure if this is really associative...
instance Semigroup Substitution where
    Subst s1 <> Subst s2 = Subst $ M.filterWithKey notIdentical $ fmap (applySubst (Subst s2)) s1 <> s2
        where
            notIdentical tv (TVar tv') | tv == tv' = False
            notIdentical _ _ = True

instance Monoid Substitution where
    mempty = Subst mempty


{- Note [lookupType for VariantConstr]
For every pass after the Qualifier, Names are unique. 
Thus, a @VariantConstr@ and a simple @Var@ can never share the same name and
using the same Map for both is safe.
-}
lookupType :: HasCallStack => QualifiedName -> TCEnv -> Type
lookupType v TCEnv{_varTypes} = lookup v _varTypes & fromMaybe (error $ "lookupType: Typechecker cannot find variable: " <> show v)

insertType :: QualifiedName -> Type -> TCEnv -> TCEnv
insertType x t env = env & varTypes %~ insert x t

typecheck :: (Trace, Members '[Fresh TVar TVar, Fresh Text QualifiedName, Error TypeError, Dump [TConstraint]] r)
          => TCEnv 
          -> Module Typecheck 
          -> Sem r (Module NextPass)
typecheck env (Module ext mname sts) = do
    (constrs, sts') <- runOutputList $ typecheckStatements env sts
    dump constrs
    subst <- solveConstraints constrs
    pure $ Module ext mname (applySubst subst sts')

typecheckStatements :: (Trace, Members '[Fresh TVar TVar, Fresh Text QualifiedName, Output TConstraint] r)
                    => TCEnv
                    -> [Statement Typecheck]
                    -> Sem r [Statement NextPass]
typecheckStatements env (Def fixity li (Decl () f xs e) expectedTy : sts) = runReader li do
    expectedTy' <- skolemize expectedTy
    (xs', eTy) <- decomposeParams expectedTy' xs
    traceM DebugVerbose $ "[typecheckStatements env (Def ...)] xs' = " <> show xs' <> " | eTy = " <> show eTy

    let env' = insertType f expectedTy env

    e' <- check (foldr (uncurry insertType) env' xs') e eTy

    (Def fixity li (Decl (expectedTy, []) f xs' e') expectedTy :) <$> typecheckStatements env' sts

typecheckStatements env (Import () li name : sts) = (Import () li name :) <$> typecheckStatements env sts 
typecheckStatements env (DefClass k li cname tvs methSigs : sts) = undefined -- (DefClass k li cname tvs methSigs :) <$> typecheckStatements env sts
typecheckStatements env (DefInstance x li cname ty meths : sts) = undefined
typecheckStatements env (DefVariant k li tyName tvs constrs : sts) =
    (DefVariant k li tyName tvs constrs' :) <$> typecheckStatements env' sts
        where
            resTy = foldl' (\x y -> TApp x (TVar y)) (TCon tyName k) tvs
            constrTy ps = TForall tvs (foldr (:->) resTy ps)
            constrs' = map (\(n, ps, (i, j)) -> (n, ps, (constrTy ps, i, j))) constrs
            env' = foldr (\(n,_,(t,_,_)) -> insertType n t) env constrs'

typecheckStatements _ [] = pure []

check :: (Trace, Members '[Output TConstraint, Fresh Text QualifiedName, Fresh TVar TVar] r)
      => TCEnv
      -> Expr Typecheck 
      -> Type 
      -> Sem r (Expr NextPass)
check env e t | trace DebugVerbose ("[check]: Γ ⊢ " <> show e <> " : " <> ppType t) False = error "unreachable"
check env e t@TForall{} = check env e =<< skolemize t -- TODO: We need to 'correct' the extension field to have type t instead of [skolemize t]
check env (App () li f x) t = runReader li do
    f' <- infer env f
    (fDomTy, fCodomTy) <- decomposeFun (getType f') 
    traceM DebugVerbose $ "[check env (FCall ...)] getType f' = " <> ppType (getType f') <> " | t = " <> ppType t <> " | fDomTy = " <> ppType fDomTy <> " | fCodomTy = " <> ppType fCodomTy
    -- (fParamTys, fResTy) <- splitType (getType f') (length xs)

    -- xs' <- for (NE.zip (fromList fParamTys) xs) \(paramTy, arg) -> do
    --    checkPoly env arg paramTy
    
    x' <- checkPoly env x fDomTy

    checkInst fCodomTy t

    pure (App t li f' x')

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
    checkInst xTy t
    pure (Var (t, []) li x)

check env (Ascription () li e (coercePass -> t1)) t2 = runReader li do
    e' <- checkPoly env e t1
    checkInst t1 t2
    pure (Ascription (coercePass t2) li e' (coercePass t1))

check env (VariantConstr (i, j) li c) t = runReader li do
    checkInst (lookupType c env) t
    pure (VariantConstr (t, i, j) li c)
check env (Case () li e branches) t = do
    e' <- infer env e
    branches' <- forM branches \(CaseBranch () li p brExpr) -> do
        (p', extendEnv) <- checkPattern p (getType e')
        brExpr' <- check (extendEnv env) brExpr t
        pure (CaseBranch () li p' brExpr')
    pure $ Case t li e' branches'
check env (Lambda () li x e) t = runReader li do    
    (expectedArgTy, expectedResTy) <- decomposeFun t
    traceM DebugVerbose $ "[check env (Lambda ...)] t = " <> ppType t <> " | expectedArgTy = " <> ppType expectedArgTy <> " | expectedResTy = " <> ppType expectedResTy

    e' <- checkPoly (insertType x expectedArgTy env) e expectedResTy

    pure (Lambda t li x e')

checkPattern :: Members '[Output TConstraint, Fresh Text QualifiedName, Fresh TVar TVar] r 
             => Pattern Typecheck
             -> Type
             -> Sem r (Pattern NextPass, TCEnv -> TCEnv)
checkPattern (IntP () n) t = pure (IntP () n, id)
checkPattern (VarP () x) t = pure (VarP t x, insertType x t)
checkPattern (ConstrP i cname ps) t = do
    
    undefined

infer :: (Trace, Members '[Output TConstraint, Fresh Text QualifiedName, Fresh TVar TVar] r )
      => TCEnv
      -> Expr Typecheck 
      -> Sem r (Expr NextPass)
-- infer env (FCall () li f xs) = runReader li do
--     f' <- infer env f
-- 
--     (fParamTys, fResTy) <- splitType (getType f') (length xs)
-- 
--     xs' <- for (NE.zip (fromList fParamTys) xs) \(paramTy, arg) -> do
--         checkPoly env arg paramTy
--     
--     t <- instantiate fResTy
--     pure (FCall t li f' xs')

infer env (App () li f x) = runReader li do
    f' <- infer env f
    (fDomTy, fCodomTy) <- decomposeFun (getType f') 
    traceM DebugVerbose $ "[infer env (FCall ...)] getType f' = " <> ppType (getType f') <> " | fDomTy = " <> ppType fDomTy <> " | fCodomTy = " <> ppType fCodomTy
    
    x' <- checkPoly env x fDomTy

    t <- instantiate fCodomTy

    pure (App t li f' x')

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
    -- equivalent (though they may not necessarily *look* the same, thanks to deep skolemization).
    subsume (getType th') (getType el')
    subsume (getType el') (getType th')

    -- We arbitrarily choose getType th' as the final type, since the types of
    -- th' and el' are equivalent.
    pure $ If (getType th') li c' th' el'
infer env (Let () li decl@(Decl () f xs e1) e2) = do
    xs' <- traverse (\x -> (x,) <$> freshTV KStar) xs

    e1' <- infer (foldr (uncurry insertType) env xs') e1

    Let () li (Decl (getType e1', []) f xs' e1') <$> infer (insertType f (getType e1') env) e2
    
infer env (Var () li x) = do
    let xTy = lookupType x env
    ty <- instantiate xTy
    pure (Var (ty, []) li x)

infer env (Ascription () li e (coercePass -> t)) = do
    e' <- checkPoly env e t
    t2 <- instantiate t
    pure (Ascription t2 li e' t)

infer env (VariantConstr (i, j) li c) = do
    cTy <- instantiate $ lookupType c env
    pure (VariantConstr (cTy, i, j) li c)

infer env (Case () li e branches) = runReader li do
    e' <- infer env e
    branches' <- forM branches \(CaseBranch () li p brExpr) -> do
        (p', extendEnv) <- checkPattern p (getType e')
        brExpr' <- infer (extendEnv env) brExpr
        pure (CaseBranch () li p' brExpr')
    

    -- We have to make sure all branches have the same return type.
    -- See 'infer env (If ...)' for a more detailed explanation
    t <- checkEquiv (map (\(CaseBranch _ _ _ expr) -> getType expr) branches')
    pure (Case t li e' branches')
    where
        checkEquiv :: forall r. Members '[Reader LexInfo, Output TConstraint, Fresh Text QualifiedName] r 
                   => [Type] 
                   -> Sem r Type
        checkEquiv []         = freshTV KStar -- the case expression is empty
        checkEquiv [t]        = pure t
        checkEquiv (t1:t2:ts) = do
            subsume t1 t2
            subsume t2 t1
            checkEquiv (t2:ts)


infer env (Lambda () li x e) = do
    xTy <- freshTV KStar

    e' <- infer (insertType x xTy env) e

    pure (Lambda (xTy :-> getType e') li x e')

checkPoly :: (Trace, Members '[Output TConstraint, Fresh Text QualifiedName, Fresh TVar TVar] r)
          => TCEnv
          -> Expr Typecheck 
          -> Type 
          -> Sem r (Expr NextPass)
checkPoly env expr ty = do
    -- (_tvs, ty') <- projection ty 
    ty' <- instantiate ty

    check env expr ty'

projection :: Members '[Fresh TVar TVar] r => Type -> Sem r ([TVar], Type)
projection (TForall tvs ty) = do
    newTVs <- traverse freshVar tvs
    first (<>newTVs) <$> projection (replaceTVars (M.fromList (zipWith (\x y -> (x, TVar y)) tvs newTVs)) ty)
projection (TFun dom codom) = second (TFun dom) <$> projection codom
projection ty = pure ([], ty)

{- note: [Generalization]
We don't perform *any* (implicit) generalization at the moment.
Top level functions have to include type signatures anyway and generalization for
local bindings is known to cause problems and is rarely that useful[1].
In the few cases where polymorphic lets are useful, it is always possible to write an explicit ascription.

Generalization for top level functions becomes quite limited as more complicated type system features are introduced and,
since even in Haskell, function signatures are strongly encouraged, it is ultimately not that useful.

[1]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf
-}


checkInst :: Members '[Output TConstraint, Reader LexInfo] r => Type -> Type -> Sem r ()
checkInst = subsume

instantiate :: Members '[Fresh TVar TVar] r => Type -> Sem r Type
instantiate (TForall tvs ty) = do
    tvMap <- M.fromList <$> traverse (\tv -> (tv,) . TVar <$> freshVar tv) tvs
    pure $ replaceTVars tvMap ty
instantiate ty = pure ty


decomposeFun :: Members '[Fresh Text QualifiedName, Fresh TVar TVar, Reader LexInfo, Output TConstraint] r 
          => Type 
          -> Sem r (Type, Type)
decomposeFun t@TForall{} = do
    t' <- instantiate t
    decomposeFun t'
decomposeFun (TFun a b) = pure (a, b)
decomposeFun t = do
    argTy <- freshTV KStar
    resTy <- freshTV KStar
    subsume t (argTy :-> resTy)
    pure (argTy, resTy)

decomposeParams :: Members '[Fresh Text QualifiedName, Fresh TVar TVar, Reader LexInfo, Output TConstraint] r
                => Type
                -> [a]
                -> Sem r ([(a, Type)], Type)
decomposeParams ty [] = pure ([], ty)
decomposeParams ty (x:xs) = do
    (argTy, resTy) <- decomposeFun ty 
    (restArgTys, restResTy) <- decomposeParams resTy xs
    pure ((x,argTy) : restArgTys, restResTy)


lambdasToDecl :: LexInfo -> QualifiedName -> Expr NextPass -> Int -> Decl NextPass
lambdasToDecl li f = go []
    where
        go :: [(QualifiedName, Type)] -> Expr NextPass -> Int -> Decl NextPass
        go args e 0                 = Decl (getType e, []) f (reverse args) e
        go args (Lambda t _ x e) n  = go ((x, argTy t):args) e (n - 1)
        go args e n                 = error $ "Typechecker.lambdasToDecl: suppplied lambda did not have enough parameters.\n  Remaining parameters: " <> show n <> "\n  Expression: " <> show e
        
        argTy (t1 :-> t2) = t1
        argTy t           = error $ "Typechecker.lambdasToDecl: Lambda type is not a function type: " <> show t


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


solveConstraints :: Members '[Error TypeError,  Fresh TVar TVar] r
                 => [TConstraint] 
                 -> Sem r Substitution
solveConstraints [] = pure mempty
solveConstraints ((MkTConstraint (Unify t1 t2) li):constrs) = runReader li do
    -- This is a bit inefficient (quadratic?).
    -- Let's make sure the constraint solver actually works, before we try to deal with that.
    subst <- unify t1 t2
    (subst <>) <$> solveConstraints (applySubst subst constrs)

solveConstraints ((MkTConstraint (Subsume t1 t2) li):constrs) = runReader li do
    subst <- subsumption t1 t2
    (subst <>) <$> solveConstraints (applySubst subst constrs)

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
unify (TSkol tv1) (TSkol tv2)
    | tv1 == tv2 = pure mempty


unify (TSkol sv) t2 = ask >>= \li -> throw $ SkolBinding li sv t2
unify t1 (TSkol sv) = ask >>= \li -> throw $ SkolBinding li sv t1
unify t1 t2         = ask >>= \li -> throw $ CannotUnify li t1 t2 

bind :: Members '[Reader LexInfo, Error TypeError] r => TVar -> Type -> Sem r Substitution
bind tv ty
    | TVar tv == ty = pure mempty
    | occurs tv ty = ask >>= \li -> throw (Occurs li tv ty)
    | TForall{} <- ty = ask >>= \li -> throw (Impredicative li tv ty)
    | otherwise = pure $ Subst (one (tv, ty))

occurs :: TVar -> Type -> Bool
occurs tv ty = tv `Set.member` freeTVs ty

subsumption :: Members '[Error TypeError, Reader LexInfo, Fresh TVar TVar] r
      => Type
      -> Type
      -> Sem r Substitution
-- subsumption t1 t2@TForall{} = do
--    t2' <- skolemize t2
--    subsumption t1 t2'
--subsumption t1@TForall{} t2 = do
--    t1' <- instantiate t1
--    subsumption t1' t2
subsumption t1 t2 = do
    t1' <- instantiate t1
    t2' <- skolemize t2
    unify t1' t2'

skolemize :: forall r. Members '[Fresh TVar TVar] r
          => Type 
          -> Sem r Type
skolemize = go mempty
    where
        go :: Map TVar Type -> Type -> Sem r Type
        go foundSkolems (TForall tvs ty) = do
            skolemMap <- M.fromList <$> traverse (\tv -> (tv,) . TSkol <$> freshVar tv) tvs
            go (foundSkolems <> skolemMap) ty
        go foundSkolems (TFun t1 t2)     = TFun (replaceTVars foundSkolems t1) <$> go foundSkolems t2
        go foundSkolems ty               = pure $ replaceTVars foundSkolems ty 


applySubst :: Data from => Substitution -> from -> from
applySubst s = transformBi \case
    TVar a' | Just t' <- lookup a' (unSubst s) -> t'
    x -> x



ppTC :: [TConstraint] -> Text
ppTC = unlines . map (\(MkTConstraint c l) -> ppTConstraint c <> "    @" <> show l) 

ppWanteds :: [TWanted] -> Text
ppWanteds = unlines . map (\(TWanted c li) -> ppConstraint c <> " @" <> show li)

ppGivens :: [TGiven] -> Text
ppGivens = unlines . map (\(TGiven c li) -> ppConstraint c <> " @" <> show li)


ppTConstraint :: TConstraintComp -> Text
ppTConstraint (Unify t1 t2)     = ppType t1 <> " ~ " <> ppType t2
ppTConstraint (Subsume t1 t2)   = ppType t1 <> " ≤ " <> ppType t2

ppType :: Type -> Text
ppType (TFun a b)           = "(" <> ppType a <> " -> " <> ppType b <> ")"
ppType (TVar (MkTVar v _))  = show v
ppType (TSkol (MkTVar v _)) = "@" <> show v
ppType (TCon v _)           = show v
ppType (TApp a b)           = "(" <> ppType a <> " " <> ppType b <> ")"
ppType (TForall ps t)       = "(∀" <> T.intercalate " " (map (\(MkTVar v _) -> show v) ps) <> ". " <> ppType t <> ")"
ppType (TConstraint c t)    = ppConstraint c <> " => " <> ppType t

ppConstraint :: Constraint -> Text
ppConstraint (MkConstraint n t) = show n <> " " <> ppType t



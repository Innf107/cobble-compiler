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

import Data.List.NonEmpty qualified as NE

type NextPass = PostProcess

data TCEnv = TCEnv {
    _varTypes :: M.Map QualifiedName Type
,   _tcInstances :: M.Map QualifiedName [Type] 
    -- ^ Once multiparam typeclasses are implemented, this will have to be @M.Map QualfiedName [[Type]]@
} deriving (Show, Eq, Generic, Data)

makeLenses ''TCEnv


data TypeError deriving (Show, Eq, Generic, Data)


data TConstraint = MkTConstraint {
    getConstraint     :: TConstraintComp 
,   constraintLexInfo :: LexInfo
} deriving (Show, Eq, Generic, Data)

data TConstraintComp = Unify Type Type      -- σ ~ ρ
                     | Subsume Type Type    -- σ ≤ ρ
                     deriving (Show, Eq, Generic, Data)

(!~) :: Members '[Writer [TConstraint], Reader LexInfo] r => Type -> Type -> Sem r ()
t1 !~ t2 = ask >>= \li -> tell [MkTConstraint (Unify t1 t2) li]
infix 1 !~

subsume :: Members '[Writer [TConstraint], Reader LexInfo] r => Type -> Type -> Sem r ()
subsume t1 t2 = ask >>= \li -> tell [MkTConstraint (Unify t1 t2) li]


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

typecheck :: Members '[Fresh TVar TVar, Fresh Text QualifiedName] r 
          => TCEnv 
          -> Module Typecheck 
          -> Sem r (Module NextPass)
typecheck = undefined

check :: Members '[Writer [TConstraint], Fresh Text QualifiedName, Fresh TVar TVar] r 
      => TCEnv
      -> Expr Typecheck 
      -> Type 
      -> Sem r (Expr NextPass)
check env (FCall () li f xs) t = runReader li do
    f' <- infer env f
    (fParamTys, fResTy) <- splitType (getType f') (length xs)

    xs' <- for (NE.zip (fromList fParamTys) xs) \(paramTy, arg) -> do
        checkPoly env arg paramTy
    
    checkInst fResTy t

    pure (FCall t li f' xs')

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
check env (Let () li decl@(Decl () x xs e1) e2) t = do
    let dlambda = foldr (Lambda () li) e1 xs

    -- See note [Generalization]
    dlambda' <- infer env dlambda
    let decl' = lambdasToDecl li x dlambda' (length xs)
    
    e2' <- check (insertType x (getType decl') env) e2 t
    
    pure $ Let () li decl' e2' 

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
check env (Case () li e branches) t = undefined
check env (StructConstruct _ _ _ _) t = error "Structs are not implemented"
check env (StructAccess _ _ _ _) t = error "Structs are not implemented"
check env (Lambda () li x e) t = runReader li do    
    expectedArgTy <- freshTV KStar
    expectedResTy <- freshTV KStar 
    
    t !~ (expectedArgTy :-> expectedResTy)

    e' <- checkPoly (insertType x expectedArgTy env) e expectedResTy

    pure (Lambda t li x e')

infer :: Members '[Writer [TConstraint], Fresh Text QualifiedName, Fresh TVar TVar] r 
      => TCEnv
      -> Expr Typecheck 
      -> Sem r (Expr NextPass)
infer env (FCall () li f xs) = runReader li do
    f' <- infer env f

    (fParamTys, fResTy) <- splitType (getType f') (length xs)

    xs' <- for (NE.zip (fromList fParamTys) xs) \(paramTy, arg) -> do
        checkPoly env arg paramTy
    
    t <- inferInst fResTy
    pure (FCall t li f' xs')

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
infer env (Let () li decl@(Decl () x xs e1) e2) = do
    let dlambda = foldr (Lambda () li) e1 xs

    -- See note [Generalization]
    dlambda' <- infer env dlambda
    let decl' = lambdasToDecl li x dlambda' (length xs)
    
    e2' <- infer (insertType x (getType decl') env) e2
    
    pure $ Let () li decl' e2' 
    
infer env (Var () li x) = do
    let xTy = lookupType x env
    ty <- inferInst xTy
    pure (Var (ty, []) li x)

infer env (Ascription () li e (coercePass -> t)) = do
    e' <- checkPoly env e t
    t2 <- inferInst t
    pure (Ascription t2 li e' t)

infer env (VariantConstr (i, j) li c) = do
    cTy <- inferInst $ lookupType c env
    pure (VariantConstr (cTy, i, j) li c)

infer env (Case () li e branches) = undefined
infer env (StructConstruct _ _ _ _) = error "Structs are not implemented"
infer env (StructAccess _ _ _ _) = error "Structs are not implemented"
infer env (Lambda () li x e) = do
    xTy <- freshTV KStar

    e' <- infer (insertType x xTy env) e

    pure (Lambda (xTy :-> getType e') li x e')

checkPoly :: Members '[Writer [TConstraint], Fresh Text QualifiedName, Fresh TVar TVar] r 
          => TCEnv
          -> Expr Typecheck 
          -> Type 
          -> Sem r (Expr NextPass)
checkPoly env expr ty = do
    -- TODO
    -- We cannot actually compute the projection of ty, can we?
    -- Since the constraint solver did not run yet, ty might just be a
    -- unification variable. 
    -- Do we instead return a type variable and some form of projection constraint?
    (_tvs, ty') <- projection ty 
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


checkInst :: Members '[Writer [TConstraint], Reader LexInfo] r => Type -> Type -> Sem r ()
checkInst = subsume

inferInst :: Members '[Fresh TVar TVar] r => Type -> Sem r Type
inferInst (TForall tvs ty) = do
    tvMap <- M.fromList <$> traverse (\tv -> (tv,) . TVar <$> freshVar tv) tvs
    pure $ replaceTVars tvMap ty
inferInst ty = pure ty


-- splitType t n = ([a1,...,an], an+1) with t <= a1 -> ... -> an+1
splitType :: Members '[Fresh Text QualifiedName, Reader LexInfo, Writer [TConstraint]] r 
          => Type 
          -> Int 
          -> Sem r ([Type], Type)
splitType t n = do
    argTys <- replicateM n (freshTV KStar)
    resTy <- freshTV KStar
    subsume t (foldr (:->) resTy argTys)
    pure (argTys, resTy)

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
replaceTVars tvs ty@(TFun a b) = TApp (replaceTVars tvs a) (replaceTVars tvs b)
replaceTVars tvs (TForall forallTVs ty) =
                let remainingTVs = foldr (M.delete) tvs forallTVs in
                replaceTVars remainingTVs ty
replaceTVars tvs (TConstraint (MkConstraint constrName constrTy) ty) =
                TConstraint (MkConstraint constrName (replaceTVars tvs constrTy)) (replaceTVars tvs ty)


solveConstraints :: Members '[Error TypeError] r
                 => [TConstraint] 
                 -> Sem r Substitution
solveConstraints [] = pure mempty
solveConstraints ((MkTConstraint (Unify t1 t2) li):constrs) = do
    -- This is a bit inefficient (quadratic?).
    -- Let's make sure the constraint solver actually works, before we try to deal with that.
    subst <- unify t1 t2 li
    (subst <>) <$> solveConstraints (applySubst subst constrs)
solveConstraints ((MkTConstraint (Subsume t1 t2) li):constrs) = undefined

unify :: Members '[Error TypeError] r
      => Type
      -> Type
      -> LexInfo
      -> Sem r Substitution
unify t1 t2 li = undefined


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
ppType (TCon v _)           = "C:" <> show v
ppType (TApp a b)           = "(" <> ppType a <> " " <> ppType b <> ")"
ppType (TForall ps t)       = "(∀" <> T.intercalate " " (map (\(MkTVar v _) -> show v) ps) <> ". " <> ppType t <> ")"
ppType (TConstraint c t)    = ppConstraint c <> " => " <> ppType t

ppConstraint :: Constraint -> Text
ppConstraint (MkConstraint n t) = show n <> " " <> ppType t



{-#LANGUAGE TemplateHaskell#-}
{-#OPTIONS_GHC -Wno-orphans#-}
module Language.Cobble.Typechecker where

import Language.Cobble.Prelude
import Language.Cobble.Util
import Language.Cobble.Util.Convert
import Language.Cobble.Util.Bitraversable
import Language.Cobble.Util.Polysemy.Fresh
import Language.Cobble.Util.Polysemy.Dump
import Language.Cobble.Types hiding (Type)
import Language.Cobble.Types qualified as C 
import Language.Cobble.Types.Lens

import qualified Data.Text as T

import qualified Data.Map as M

type NextPass = PostProcess

type Type = C.Type NextPass

data TCState = TCState {
    _varTypes :: M.Map QualifiedName Type
,   _tcInstances :: M.Map QualifiedName [Type] 
    -- ^ Once multiparam typeclasses are implemented, this will have to be @M.Map QualfiedName [[Type]]@
} deriving (Show, Eq, Generic, Data)
instance Semigroup TCState where
    (TCState t1 i1) <> (TCState t2 i2) = TCState (t1 <> t2) (i1 <> i2)
instance Monoid TCState where
    mempty = TCState mempty mempty

makeLenses ''TCState

data TypeError = DifferentConstructor LexInfo Type Type 
               | NotEnoughArgs LexInfo Type Type
               | Occurs LexInfo (TVar NextPass) Type
               | HigherPoly LexInfo Type Type
               | NoStructsForType LexInfo Type
               | AmbiguousStructAccess LexInfo Type [Type]
               | SkolBinding LexInfo Type Type
               | NoInstanceFor LexInfo (Constraint NextPass)
               | AmbiguousInstanceFor LexInfo (Constraint NextPass) [TGiven]
               deriving (Show, Eq, Generic, Data)

type TConstraint = (TConstraintComp, LexInfo)


data TConstraintComp = Type :~ Type
                     | OneOf Type [Type]
                     deriving (Show, Eq, Generic, Data)


newtype Substitution = Subst {unSubst :: Map (TVar NextPass) Type} 
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
lookupType :: HasCallStack => Members '[State TCState, Fresh (TVar NextPass) (TVar NextPass), Reader LexInfo] r => QualifiedName -> Sem r Type
lookupType v = ask >>= \li -> gets (lookup v . view varTypes) <&> fromMaybe (error $ "lookupType: Typechecker cannot find variable: " <> show v <> " @" <> show li)

instantiate :: forall r. Members '[Fresh (TVar NextPass) (TVar NextPass), Writer [TWanted]] r => LexInfo -> Type -> Sem r Type
instantiate li ty = fst <$> instantiateWithWanteds li ty

instantiateWithWanteds :: forall r. Members '[Fresh (TVar NextPass) (TVar NextPass), Writer [TWanted]] r => LexInfo -> Type -> Sem r (Type, [TWanted])
instantiateWithWanteds li = \case
    (TForall ps t) -> instantiateConstraints =<< foldrM (\p r -> freshVar p <&> \p' -> replaceTVar p (TVar p') r) t ps
    ty -> instantiateConstraints ty
    where
        instantiateConstraints :: Type -> Sem r (Type, [TWanted])
        instantiateConstraints (TConstraint c ty) = do
            let wanted = TWanted c li
            tell [wanted]
            (second (wanted:)) <$> instantiateConstraints ty
        instantiateConstraints x = pure (x, [])

-- currently only skolemizes top level foralls (since higher-ranked polymorphism is not implemented yet)
skolemize :: forall r. Members '[Fresh (TVar NextPass) (TVar NextPass), Writer [TGiven]] r => LexInfo -> Type -> Sem r Type
skolemize li (TForall ps t) = skolemizeConstraints $ foldr (\p r -> replaceTVar p (TSkol p) r) t ps
    where 
        skolemizeConstraints :: Type -> Sem r Type 
        skolemizeConstraints (TConstraint c ty) = do
            tell [TGiven c li]
            skolemizeConstraints ty
        skolemizeConstraints x = pure x
skolemize _ x              = pure x

insertType :: Members '[State TCState] r => QualifiedName -> Type -> Sem r ()
insertType v n = modify (varTypes %~ insert v n)

insertInstance :: Members '[State TCState, Writer [TGiven]] r => LexInfo -> QualifiedName -> Type -> Sem r ()
insertInstance li v t = addToState >> emitGiven
    where  
        addToState = modify $ tcInstances . at v %~ \case
            Nothing -> Just [t]
            Just ts -> Just (t:ts)
        emitGiven = tell [TGiven (MkConstraint v t) li]

getType' :: (Members '[Fresh (TVar NextPass) (TVar NextPass), Writer [TWanted]] r, HasType a NextPass, HasLexInfo a) 
         => a 
         -> Sem r Type
getType' x = getTypeWith' (getLexInfo x) x

getTypeWith' :: (Members '[Fresh (TVar NextPass) (TVar NextPass), Writer [TWanted]] r, HasType a NextPass) 
             => LexInfo 
             -> a 
             -> Sem r Type
getTypeWith' li = instantiate li . getType

checkStmnt :: Members '[Writer [TConstraint], Writer [TWanted], Writer [TGiven], Fresh Text QualifiedName, Fresh (TVar NextPass) (TVar NextPass), State TCState] r 
           => Statement Typecheck 
           -> Sem r (Statement NextPass)
checkStmnt (Import () li mod) = pure $ Import () li mod
checkStmnt (DefStruct k li sname ps fields) = pure $ DefStruct k li sname (map coercePass ps) (map (second coercePass) fields) 
checkStmnt (DefVariant k li sname (coercePass -> ps) cs) = do
    let appliedType :: Type = foldl' TApp (TCon sname k) (map TVar ps)
    cs' <- forM cs \(cname, coercePass -> fs, (ep, ix)) -> do
        let constrTy = TForall ps (foldr (:->) appliedType fs )
        insertType cname constrTy
        pure (cname, fs, (constrTy, ep, ix))
    pure (DefVariant k li sname (coercePass ps) cs')
checkStmnt (DefClass k li cname ps meths) = do
    -- Assumes that implicit foralls and constraints have been added
    -- in SemAnalysis
    forM_ meths \(mname, mtype) -> do
        insertType mname (coercePass mtype)
    
    pure $ DefClass k li cname (coercePass ps) (map (second coercePass) meths)
checkStmnt (DefInstance (defs, classPs) li cname (coercePass -> ty) decls) = do
    insertInstance li cname ty
    decls' <- forM (zip defs decls) \((fname, coercePass -> declTy), d@(Decl () declFname _ _)) -> do
        -- sanity check
        when (fname /= declFname) $ error $ "checkStmnt: Tyclass instance for '" <> show cname <> "' was not properly reordered"
        
        let declTy' = case coercePass classPs of
                [p] -> replaceTVar p ty declTy
                _ -> error $ "checkStmnt: Multiparam instances NYI: " <> show classPs

        (givens, ty') <- tapAssocR @[TGiven] $ skolemize li declTy'
        -- traceM $ show declFname <> ": " <> show givens <> " arising from: " <> toString (ppType declTy')
        decl' <- checkDecl d givens
        tellLI li [ty' :~ (getType decl')]
        pure decl'
    pure (DefInstance (map (second coercePass) defs, coercePass classPs) li cname (coercePass ty) decls')

checkStmnt (Def mfixity li decl@(Decl () f _ _) (coercePass -> ty)) = do
    insertType f ty
    (givens, ty') <- tapAssocR @[TGiven] $ skolemize li ty
    decl' <- checkDecl decl givens
    tellLI li [ty' :~ getType decl']
    pure (Def mfixity li decl' ty)

checkDecl :: Members '[Writer [TConstraint], Writer [TWanted], Writer [TGiven], Fresh Text QualifiedName, Fresh (TVar NextPass) (TVar NextPass), State TCState] r
          => Decl Typecheck 
          -> [TGiven]
          -> Sem r (Decl NextPass)
checkDecl (Decl () f ps e) gs = do
    psTys <- traverse (\_ -> freshTV KStar) ps
    zipWithM_ insertType ps psTys
    e' <- check e
    eTy <- getType' e'
    let resTy = foldr (:->) eTy psTys
    pure (Decl (resTy, gs) f (zip ps psTys) e')

check :: Members '[Writer [TConstraint], Writer [TWanted], Writer [TGiven], Fresh Text QualifiedName, Fresh (TVar NextPass) (TVar NextPass), State TCState] r 
      => Expr Typecheck 
      -> Sem r (Expr NextPass)
check (IntLit () li n)   = pure (IntLit () li n)
check (UnitLit li)              = pure (UnitLit li)
check (Var () li vname)  = runReader li do
    (ty, wanteds) <- instantiateWithWanteds li =<< lookupType vname
    -- traceM (show vname <> ": " <> show wanteds)
    pure $ Var (ty, wanteds) li vname
-- See note [lookupType for VariantConstr]
check (VariantConstr (e,i) li cname) = runReader li $ VariantConstr . (,e,i) <$> (instantiate li =<< lookupType cname) <*> pure li <*> pure cname
check (Case () li e cases) = do
    e' <- check e
    beta <- freshTV KStar
    cases' <- forM cases \(CaseBranch () brLi brPat brExpr) -> runReader brLi do
        brPat' <- checkPattern brPat
        brExpr' <- check brExpr
        tellLI brLi [getType brPat' :~ getType e', getType brExpr' :~ beta]
        pure (CaseBranch () brLi brPat' brExpr')
    pure (Case beta li e' cases')
check (Ascription () li e ty) = do
    e' <- check e
    ty' <- skolemize li (coercePass ty)
    tellLI li [getType e' :~ ty']
    pure e' -- Ascriptions are removed after type checking
check (FCall () li f as) = do
    f' <- check f
    as' <- traverse check as
    ret <- freshTV KStar

    fTy <- getType' f'

    asTys <- traverse getType' as'

    tellLI li [fTy :~ (foldr (:->) ret asTys)]
    pure (FCall ret li f' as')
check (If () li cond th el) = do
    cond' <- check cond
    condTy <- getType' cond'
    
    th' <- check th
    thTy <- getType' th'
    
    el' <- check el
    elTy <- getType' el'

    tellLI li [condTy :~ boolT, thTy :~ elTy]
    pure (If () li cond' th' el')
check (Let () li (Decl () f ps e) body) = do
    fTy <- freshTV KStar
    psTys <- traverse (\_ -> freshTV KStar) ps

    insertType f fTy

    zipWithM_ insertType ps psTys
    e' <- check e
    eTy <- getType' e'
    
    body' <- check body

    tellLI li [fTy :~ foldr (:->) eTy psTys]
    -- TODO: Let removes all constraints, which is intended, but we need to make sure
    -- that all constraints are properly instantiated 
    -- (which should probably be fine thanks to the lack of generalization).
    pure (Let () li (Decl (fTy, []) f (zip ps psTys) e') body')
check (StructConstruct structDef li structName fields) = do
    let sTy = coercePass $ structDef ^. structType
    
    psTys <- traverse (freshVar . coercePass @(TVar Typecheck) @(TVar PostProcess)) (structDef ^. structParams)
    let polyTypes :: M.Map (TVar NextPass) (TVar NextPass) = fromList $ zipWith (\x y -> (x, coercePass y)) psTys (structDef ^. structParams) 
    -- Application is left associative
    let retTy = foldl' (\r tv -> TApp r (TVar tv)) sTy psTys

    fields' <- zipForM (structDef ^. structFields) fields \(name, (coercePass -> fieldType)) (_, fieldExpr) -> do
        let fieldType' = fieldType & transform \case
                TVar tv | Just tv' <- lookup tv polyTypes -> TVar tv'
                x -> x
        fieldExpr' <- check fieldExpr
        exprTy <- getType' fieldExpr'
        
        tellLI li [exprTy :~ fieldType']

        pure (name, fieldExpr')

    pure (StructConstruct (coercePass structDef, retTy) li structName fields')

check (StructAccess possibleStructs li sexpr field) = do
    sexpr' <- check sexpr
    retTy <- freshTV KStar

    sexprTy <- getType' sexpr'

    let structTys = map (coercePass . view structType) (toList possibleStructs) 

    tellLI li [OneOf sexprTy structTys]
    -- Codegen needs the correct StructDef, but we don't know that until the constraint solver is done
    pure (StructAccess (coercePass possibleStructs, sexprTy, retTy) li sexpr' field)
check (Lambda () li x e) = do
    xType <- freshTV KStar
    insertType x xType

    e'  <- check e

    pure $ Lambda (xType :-> getType e') li x e'




checkPattern :: Members '[Writer [TConstraint], Writer [TWanted], Fresh Text QualifiedName, Fresh (TVar NextPass) (TVar NextPass), State TCState, Reader LexInfo] r
             => Pattern Typecheck
             -> Sem r (Pattern NextPass)
checkPattern (IntP () n) = pure (IntP () n)
checkPattern (VarP () n) = do 
    alpha <- freshTV KStar
    insertType n alpha
    pure (VarP alpha n)
checkPattern (ConstrP i n ps) = ask >>= \li -> do
    ps' <- traverse checkPattern ps
    alpha <- freshTV KStar
    -- See note [lookupType for VariantConstr]
    constrTy <- instantiate li =<< lookupType n
    tellLI li [foldr (:->) alpha (map getType ps') :~ constrTy]
    pure (ConstrP (alpha, i) n ps')

typecheck :: Members '[Error TypeError, Fresh Text QualifiedName, State TCState, Dump [TConstraint], Dump [TGiven], Dump [TWanted], Output Log] r 
          => Module Typecheck 
          -> Sem r (Module NextPass)
typecheck (Module deps mname sts) = do
    (constraints, (givens, (wanteds, sts'))) <- runWriterAssocR @[TConstraint] 
                                            $ runWriterAssocR @[TGiven] 
                                            $ runWriterAssocR @[TWanted]
                                            $ runFreshM freshenTV 
                                            $ traverse checkStmnt sts
    
    dump constraints
    subst <- solve mempty constraints
    let givens'  = applySubst subst givens
        wanteds' = applySubst subst wanteds
    dump givens'
    dump wanteds'
    subst' <- solveGivensWanteds givens' wanteds' subst
    pure $ Module deps mname (applySubst subst' sts')
        where
            freshenTV :: forall r. Members '[Fresh Text QualifiedName] r => TVar NextPass -> Sem r (TVar NextPass)
            freshenTV (MkTVar x k) = MkTVar <$> freshVar (originalName x) <*> pure k 

solve :: Members '[Error TypeError, Output Log] r => Substitution -> [TConstraint] -> Sem r Substitution
solve s (((applySubst s -> t1) :~ (applySubst s -> t2), li):cs) = do
    log LogDebugVerbose $ "Solving constraint: " <> ppTConstraint (t1 :~ t2)
    s' <- runReader li $ unify t1 t2
    solve (s <> s') cs 

solve s ((OneOf (applySubst s -> t1) (map (applySubst s) -> ts), li):cs) = do
    (rights <$> traverse (\t -> fmap (,t) <$> runError (runReader li (unify t1 t))) ts) >>= \case
        [(s', _)] -> solve (s <> s') cs
        []  -> throw $ NoStructsForType li t1
        ts'  -> throw $ AmbiguousStructAccess li t1 (map snd ts')

solve s [] = pure s

unify :: Members '[Reader LexInfo, Error TypeError, Output Log] r => Type -> Type -> Sem r Substitution
unify t1 t2 = runReader [] $ unify' t1 t2

unify' :: Members '[Reader LexInfo, Reader [Constraint NextPass], Error TypeError, Output Log] r => Type -> Type -> Sem r Substitution
unify' t1 t2 = do
    s <- unify'' t1 t2
    log LogDebugVeryVerbose $ "Unified: " <> ppType t1 <> " ~ " <> ppType t2 <> "\n    -> " <> show s 
    pure s

unify'' :: Members '[Reader LexInfo, Reader [Constraint NextPass], Error TypeError, Output Log] r => Type -> Type -> Sem r Substitution
unify'' t1@(TCon c1 _k1) t2@(TCon c2 _k2)
    | c1 == c2 = pure mempty
    | otherwise = throwLI \li -> DifferentConstructor li t1 t2
unify'' (TVar tv) t2              = bind tv t2
unify'' t1 (TVar tv)              = bind tv t1
unify'' (TApp c1 a1) (TApp c2 a2) = do
    s <- unify' c1 c2
    (s <>) <$> unify' (applySubst s a1) (applySubst s a2)

-- Skolems only unify with themselves
unify'' (TSkol tv1) (TSkol tv2)
    | tv1 == tv2 = pure mempty

unify'' t1@TCon{}    t2@TApp{}    = throwLI \li -> NotEnoughArgs li t1 t2
unify'' t1@TApp{}    t2@TCon{}    = throwLI \li -> NotEnoughArgs li t1 t2
unify'' t1@TForall{} t2           = throwLI \li -> HigherPoly li t1 t2
unify'' t1           t2@TForall{} = throwLI \li -> HigherPoly li t1 t2

unify'' t1@TConstraint{} t2      = throwLI \li -> HigherPoly li t1 t2
unify'' t1 t2@TConstraint{}      = throwLI \li -> HigherPoly li t1 t2

unify'' t1@TSkol{}   t2           = throwLI \li -> SkolBinding li t1 t2
unify'' t1           t2@TSkol{}   = throwLI \li -> SkolBinding li t1 t2

solveGivensWanteds :: Members '[Error TypeError] r
                   => [TGiven]
                   -> [TWanted]
                   -> Substitution
                   -> Sem r Substitution
solveGivensWanteds givens (w@(TWanted c li) : wanteds) subst = case mapMaybe (\g -> if constraintApplies g w then Just g else Nothing) givens of
    [_] -> solveGivensWanteds givens wanteds subst
    [] -> throw $ NoInstanceFor li c
    is  | allEqual (map (\(TGiven c _) -> c) is) -> solveGivensWanteds givens wanteds subst
        | otherwise -> throw $ AmbiguousInstanceFor li c is
solveGivensWanteds _ [] subst = pure subst

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x:y:xs) = x == y && allEqual (y:xs)

constraintApplies :: TGiven -> TWanted -> Bool
constraintApplies (TGiven (MkConstraint gc gt) _) (TWanted (MkConstraint wc wt) _) 
    | gc == wc  = morePolymorphic gt wt
    | otherwise = False
    where
        morePolymorphic :: Type -> Type -> Bool
        morePolymorphic (TCon c1 _) (TCon c2 _)     = c1 == c2
        morePolymorphic (TCon _ _) _                = False
        morePolymorphic (TApp a1 b1) (TApp a2 b2)   = morePolymorphic a1 a2 && morePolymorphic b1 b2
        morePolymorphic (TApp _ _) _                = False
        morePolymorphic (TVar _) _                  = True
        morePolymorphic (TSkol v1) (TSkol v2)       = v1 == v2
        morePolymorphic (TSkol _) _                 = False
        morePolymorphic (TForall _ _) _             = error "morePolymorphic: foralls in constraint are not implemented!"
        morePolymorphic (TConstraint _ _) _         = error "morePolymorphic: nested constraints are not implemented!"


bind :: Members '[Reader LexInfo, Reader [Constraint NextPass], Error TypeError] r => TVar NextPass -> Type -> Sem r Substitution
bind tv t
    | occurs tv t   = throwLI (\li -> Occurs li tv t)
    | TVar tv == t  = pure mempty
    | otherwise     = ask <&> \cs -> (Subst (one (tv, addConstraints cs t)))

addConstraints :: [Constraint NextPass] -> Type -> Type
addConstraints cs t = foldr TConstraint t cs

occurs :: TVar NextPass -> Type -> Bool
occurs _ TVar{} = False
occurs tv t = tv `elem` [tv' | TVar tv' <- universeBi t]


freshTV :: Members '[Fresh Text QualifiedName] r => Kind -> Sem r Type
freshTV k = freshVar "u" <&> \u -> TVar (MkTVar u k) 

replaceTVar :: TVar NextPass -> Type -> Type -> Type
replaceTVar a b = transformBi \case
    TVar a' | a == a' -> b
    c -> c

applySubst :: Data from => Substitution -> from -> from
applySubst s = transformBi \case
    TVar a' | Just t' <- lookup a' (unSubst s) -> t'
    x -> x

tellLI :: (Functor f, Members '[Writer (f (a, LexInfo))] r) => LexInfo -> f a -> Sem r ()
tellLI li xs = tell (fmap (,li) xs)

throwLI :: Members '[Reader LexInfo, Error e] r => (LexInfo -> e) -> Sem r a
throwLI e = ask >>= throw . e



ppTC :: [TConstraint] -> Text
ppTC = unlines . map (\(c, l) -> ppTConstraint c <> "    @" <> show l) 

ppWanteds :: [TWanted] -> Text
ppWanteds = unlines . map (\(TWanted c li) -> ppConstraint c <> " @" <> show li)

ppGivens :: [TGiven] -> Text
ppGivens = unlines . map (\(TGiven c li) -> ppConstraint c <> " @" <> show li)


ppTConstraint :: TConstraintComp -> Text
ppTConstraint (t1 :~ t2)     = ppType t1 <> " ~ " <> ppType t2
ppTConstraint (OneOf t1 ts)  = ppType t1 <> " ∈ {" <> T.intercalate ", " (map ppType ts) <> "}"

ppType :: Type -> Text
ppType (a :-> b)            = "(" <> ppType a <> " -> " <> ppType b <> ")"
ppType (TVar (MkTVar v _))  = show v
ppType (TSkol (MkTVar v _)) = "@" <> show v
ppType (TCon v _)           = "C:" <> show v
ppType (TApp a b)           = "(" <> ppType a <> " " <> ppType b <> ")"
ppType (TForall ps t)       = "(∀" <> T.intercalate " " (map (\(MkTVar v _) -> show v) ps) <> ". " <> ppType t <> ")"
ppType (TConstraint c t)    = ppConstraint c <> " => " <> ppType t

ppConstraint :: Constraint NextPass -> Text
ppConstraint (MkConstraint n t) = show n <> " " <> ppType t

extractConstraints :: Type -> [Constraint NextPass]
extractConstraints (TForall _ t) = extractConstraints t
extractConstraints (TConstraint c t) = c : extractConstraints t
extractConstraints _ = []

tapWith :: (Members '[Writer o] r) => (Sem (Writer o : r) a -> Sem r (o, a)) -> Sem (Writer o : r) a -> Sem r (o, a)
tapWith runW x = runW x >>= \(o, res) -> tell o >> pure (o, res)

tapAssocR :: (Members '[Writer o] r, Monoid o) => Sem (Writer o : r) a -> Sem r (o, a)
tapAssocR = tapWith runWriterAssocR


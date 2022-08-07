{-#LANGUAGE TemplateHaskell#-}
module Cobble.Qualifier where

import Cobble.Prelude
import Cobble.Syntax

import Cobble.Util.Polysemy.StackState
import Cobble.Util.Polysemy.Fresh
import Cobble.Util.Bitraversable
import Cobble.Util.TypeUtils

type NextPass = SemAnalysis

data QualificationError = VarNotFound LexInfo Text
                        | FixityNotFound LexInfo Text
                        | VariantConstrNotFound LexInfo Text
                        | TypeNotFound LexInfo Text
                        | TVarNotFound LexInfo UnqualifiedName
                        | TVarsNotFound LexInfo (Seq TVar)
                        | EffOpNotFound LexInfo UnqualifiedName
                        | InstanceForNonClass LexInfo QualifiedName Kind TypeVariant
                        | NonClassInConstraint LexInfo QualifiedName Kind TypeVariant
                        | StructConstructNotAStruct LexInfo QualifiedName Kind TypeVariant
                        | AsymmetricOrPatternVar LexInfo Text
                        deriving (Show, Eq)

data Scope = Scope {
    _scopeVars :: Map Text QualifiedName
,   _scopeTypes :: Map Text (QualifiedName, Kind, TypeVariant, Bool)
--                                                             ^ is imported
,   _scopeTVars :: Map Text (QualifiedName, Kind)
,   _scopeVariantConstrs :: Map Text (QualifiedName, Int, Int, TypeVariant)
,   _scopeFixities :: Map Text Fixity
,   _scopeEffOperations :: Map Text QualifiedName
}

instance Semigroup Scope where
    s1 <> s2 = Scope {
        _scopeVars = _scopeVars s1 <> _scopeVars s2
    ,   _scopeTypes = _scopeTypes s1 <> _scopeTypes s2
    ,   _scopeTVars = _scopeTVars s1 <> _scopeTVars s2
    ,   _scopeVariantConstrs = _scopeVariantConstrs s1 <> _scopeVariantConstrs s2
    ,   _scopeFixities = _scopeFixities s1 <> _scopeFixities s2
    ,   _scopeEffOperations = _scopeEffOperations s1 <> _scopeEffOperations s2
    }

instance Monoid Scope where
    mempty = Scope mempty mempty mempty mempty mempty mempty

makeLenses ''Scope

qualify :: (Trace, Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError] r)
        => Module QualifyNames 
        -> Sem r (Module NextPass)
qualify (Module deps name stmnts) = runReader (MkTagged name) $ Module deps name <$> traverse qualifyStmnt stmnts

qualifyStmnt :: (Trace, Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError, Reader (Tagged "ModName" Text)] r)
             => Statement QualifyNames
             -> Sem r (Statement NextPass)
qualifyStmnt (Def mfixity li d@(Decl _ n _ _) ty) = runReader li do 
    n' <- freshGlobal n
    addVar n n'
    addMFixity n mfixity
    withFrame do
        -- The type has to be qualified first, since all mentioned tyvars
        -- need to be in scope in the body (mostly for ascriptions).
        ty' <- qualifyType True ty
        Def mfixity li
            <$> qualifyDeclWithName n' d
            <*> pure ty'

qualifyStmnt (DefVariant () li n tvs constrs) = runReader li $ do
    n' <- freshGlobal n
    tvs' <- traverse (uncurry qualifyTVar) tvs
    let k = getTyConKind tvs'
    constrs' <- withFrame $ flip traverseWithIndex constrs \i (cn, tys, ()) -> do
        -- The type needs to be locally added as a dummy variant to allow recursive types.
        addType n n' k (VariantType (coercePass tvs') [])
        zipWithM_ addTVar (map fst tvs) tvs'

        cn' <- freshGlobal cn
        tys' <- traverse (qualifyType False) tys
        pure (cn', tys', (length tys', i))
    let typeVariant = (VariantType (coercePass tvs') (map (\(cn, ts, _) -> (cn, coercePass ts)) constrs'))
    addType n n' k typeVariant
    zipWithM_ (\(cn, _, _) (cn', _, (ep, i)) -> addVariantConstr cn cn' ep i typeVariant) constrs constrs'
    pure (DefVariant k li n' tvs' constrs')

qualifyStmnt (DefClass () li className tvs meths) = runReader li $ do
    className' <- freshGlobal className
    tvs' <- traverse (uncurry qualifyTVar) tvs
    let k = foldr (\(MkTVar _ k) r -> k `KFun` r) KConstraint tvs'
    -- Again, the methods have to be stubbed out since we don't have them yet, but we
    -- need the tyclass type in order to qualify them
    meths' <- withFrame $ do
        addType className className' k (TyClass (coercePass tvs') [])
        zipWithM_ addTVar (map fst tvs) tvs'
        forM meths \(mn, mty) -> withFrame do
            mn' <- freshGlobal mn
            methTy' <- qualifyType True (addConstraint mty) -- Type class methods *are* allowed to introduce new tyvars
            pure (mn', foldr TForall methTy' tvs')
    addType className className' k (TyClass tvs' meths')
    zipWithM_ (\(methName,_) (methName',_) -> addVar methName methName') meths meths'
    pure (DefClass k li className' tvs' meths')
        where
            addConstraint ty = case tvs of
                [(utv, _)] -> UTConstraint (MkUConstraint className (UTTyVar utv)) ty
                _ -> error "qualifyStmnt: Multiparam typeclasses NYI"

qualifyStmnt (DefInstance () li cname ty meths) = runReader li $ lookupType cname >>= \case
    (cname', k, TyClass tvs classMeths, isImported) -> withFrame do
        ty' <- qualifyType True ty 
        meths' <- forM meths \d@(Decl _ n _ _) -> do
            n' <- lookupVar n
            qualifyDeclWithName n' d
        pure (DefInstance (k, classMeths, tvs, isImported) li cname' ty' meths')
    (cname', k, tv, _) -> throw $ InstanceForNonClass li cname' k tv

qualifyStmnt (DefEffect () li effName tvs ops) = runReader li do
    effName' <- freshGlobal effName
    tvs' <- traverse (uncurry qualifyTVar) tvs
    
    let k = foldr (\(MkTVar _ k) r -> k `KFun` r) KEffect tvs'

    ops' <- withFrame $ do
        -- Just like with variants and classes, the operations have to be stubbed out since we don't have them yet, but we
        -- need the effect type in order to qualify them
        addType effName effName' k (TyEffect (coercePass tvs') [])
        zipWithM_ addTVar (map fst tvs) tvs'
        forM ops \(opName, opTy) -> do
            opName' <- freshGlobal opName
            opTy' <- qualifyType True opTy -- Effect operations *are* allowed to introduce new tyvars
            traceM TraceQualify $ "[qualifyStmnt (DefEffect " <> show effName <> ")]: " <> show opName' <> " : " <> show opTy'
            pure (opName', opTy')

    addType effName effName' k (TyEffect tvs' ops')
    zipWithM (\(op, _) (op', _) -> addEffOp op op') ops ops'

    zipWithM (\(opName, _) (opName', _) -> addVar opName opName') ops ops'
    pure (DefEffect k li effName' tvs' ops')

-- | Same as qualifyDecl, but takes the already qualified name as an argument
-- instead of recomputing it
qualifyDeclWithName :: (Trace, Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError, Reader LexInfo] r)
            => QualifiedName
            -> Decl 'QualifyNames 
            -> Sem r (Decl NextPass)
qualifyDeclWithName n' (Decl () _ ps e) = withFrame do
    li <- ask
    ps' <- traverse freshVar ps
    zipWithM_ addVar ps ps'
    e' <- qualifyExpr e
    pure (Decl () n' ps' e')

qualifyRecursiveLocalDecl :: (Trace, Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError, Reader LexInfo] r)
                     => Decl 'QualifyNames 
                     -> Sem r (Decl NextPass)
qualifyRecursiveLocalDecl d@(Decl _ n _ _) = do
    li <- ask
    n' <- freshVar n
    addVar n n'
    qualifyDeclWithName n' d

qualifyExpr :: (Trace, Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError] r)
            => Expr QualifyNames
            -> Sem r (Expr NextPass)
qualifyExpr (App () li f e) =
    App () li
    <$> qualifyExpr f
    <*> qualifyExpr e
qualifyExpr (IntLit () li n) = pure (IntLit () li n)
qualifyExpr (If () li cond th el) = 
    If () li
    <$> qualifyExpr cond
    <*> qualifyExpr th
    <*> qualifyExpr el
qualifyExpr (Let () li decl b) = runReader li $
    withFrame $ 
        Let () li
        <$> qualifyRecursiveLocalDecl decl
        <*> qualifyExpr b
qualifyExpr (Var () li x) = runReader li $ Var () li <$> lookupVar x
qualifyExpr (Ascription () li expr ty) = runReader li $
    Ascription () li 
    <$> qualifyExpr expr
    <*> qualifyType False ty
qualifyExpr (VariantConstr () li cname) = runReader li do
    (cname', ep, i, v) <- lookupVariantConstr cname
    pure $ VariantConstr (ep, i) li cname'
qualifyExpr (Case () li exp cases) = runReader li $ 
    Case () li
    <$> qualifyExpr exp
    <*> traverse qualifyCaseBranch cases
qualifyExpr (Lambda () li x e) = runReader li $
    withFrame $ do
        x' <- freshVar x
        addVar x x'
        Lambda () li x' <$> qualifyExpr e
qualifyExpr (Handle () li expr cases mreturnClause) = runReader li $
    Handle () li
    <$> qualifyExpr expr
    <*> traverse qualifyEffHandler cases
    <*> qualifiedReturnClause
    where
        qualifyEffHandler :: Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError, Reader LexInfo] r
                          => EffHandler QualifyNames
                          -> Sem r (EffHandler NextPass)
        qualifyEffHandler (EffHandler () li op args expr) =
            withFrame do
                op' <- lookupEffOp op
                args' <- traverse (freshVar @Text @QualifiedName) args
                zipWithM addVar args args'
                expr' <- qualifyExpr expr
                pure (EffHandler () li op' args' expr')
        qualifiedReturnClause = case mreturnClause of
            Nothing -> pure Nothing
            Just (var, expr) -> withFrame do
                var' <- freshVar var
                addVar var var'
                Just . (var',) <$> qualifyExpr expr
                
qualifyExpr (Resume () li expr) = Resume () li <$> qualifyExpr expr

qualifyExpr (ExprX (Right UnitLit) li) = pure $ VariantConstr (0, 0) li (UnsafeQualifiedName "Unit" (GlobalQName "Data.Unit"))
qualifyExpr (ExprX (Left opGroup) li) = runReader li $ replaceOpGroup . reorderByFixity <$> qualifyWithFixity opGroup
    where
        qualifyWithFixity :: Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError, Reader LexInfo] r
                          => OperatorGroup QualifyNames NoFixity 
                          -> Sem r (OperatorGroup NextPass WithFixity)
        qualifyWithFixity (OpLeaf e)            = OpLeaf <$> qualifyExpr e
        qualifyWithFixity (OpNode l (op, ()) r) = do
            f <- lookupFixity op
            op' <- lookupVar op
            OpNode
                <$> qualifyWithFixity l
                <*> pure (op', f)
                <*> qualifyWithFixity r

        replaceOpGroup :: OperatorGroup NextPass WithFixity -> Expr NextPass
        replaceOpGroup (OpLeaf e) = e
        replaceOpGroup (OpNode l (op, _fixity) r) = App () li (App () li (Var () li op) (replaceOpGroup l)) (replaceOpGroup r)
        -- See note [Fixity Algorithm]
        reorderByFixity :: OperatorGroup NextPass WithFixity -> OperatorGroup NextPass WithFixity
        reorderByFixity (OpLeaf e) = OpLeaf e
        reorderByFixity (OpNode l op             (reorderByFixity -> (OpLeaf e))) 
            = OpNode l op (OpLeaf e)
        reorderByFixity (OpNode l op@(_, fixity) (reorderByFixity -> (OpNode l' op'@(_, fixity') r')))
            --                                   left rotation
            | fixity `lowerPrecedence` fixity' = OpNode (reorderByFixity (OpNode l op l')) op' r'
            --                                   nothing
            | otherwise                        = OpNode l op (OpNode l' op' r')
        reorderByFixity _ = error "unreachable"

qualifyCaseBranch :: (Trace, Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError] r)
                  => CaseBranch QualifyNames
                  -> Sem r (CaseBranch NextPass)
qualifyCaseBranch (CaseBranch () li pat expr) = runReader li $
    withFrame $ CaseBranch () li
        <$> qualifyPattern pat
        <*> qualifyExpr expr


qualifyPattern :: Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError, Reader LexInfo] r
               => Pattern QualifyNames
               -> Sem r (Pattern NextPass)
qualifyPattern = evalState mempty . go Nothing
    where
        go :: Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError, Reader LexInfo, State (Map Text QualifiedName)] r
           => Maybe (Map Text QualifiedName) -> Pattern QualifyNames -> Sem r (Pattern NextPass)
        go _ (IntP () n) = pure (IntP () n)

        go (Just orPats) (VarP () x) = case lookup x orPats of
            Just x' -> pure (VarP () x')
            Nothing -> ask >>= \li -> throw $ AsymmetricOrPatternVar li x
            
        go Nothing (VarP () x) = do
            li <- ask
            x' <- freshVar x
            addVar x x'
            modify (insert x x')
            pure (VarP () x')

        go mOrPats (ConstrP () cname ps) = do
            (cname, _, i, v) <- lookupVariantConstr cname
            ConstrP (i, v) cname <$> traverse (go mOrPats) ps

        go _ (OrP () Empty) = error "qualifyPattern: empty or pattern" 

        go mOrPats (OrP () (p :<| pats)) = do
            (firstBoundVars, p') <- runState mempty $ go mOrPats p

            pats' <- traverse (go (mOrPats <> Just firstBoundVars)) pats
            pure $ OrP () (p' :<| pats')

        go _ (WildcardP ()) = pure (WildcardP ())

qualifyType :: forall r. (Trace, Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError, Reader LexInfo] r)
            => Bool
            -> UType
            -> Sem r Type
qualifyType allowFreeVars uty = do
    (ty, effVars, freeVars) <- go uty
    case (allowFreeVars, freeVars) of
        (False, (_:<|_)) -> ask >>= \li -> throw $ TVarsNotFound li freeVars
        _ -> pure $ foldr TForall ty (freeVars <> effVars)
    where
        -- @go@ returns two distinct sequences of tvars.
        -- The second one captures all free type variables *that have been explicitly written by the user*.
        -- These are propagated all the way up to @qualifyType@, where they are written into a top-level forall.
        -- The first sequence captures all free effect variables, that were introduced by opening closed effect rows.
        -- (See docs/RowOpening.md)
        -- These were *not* written by the user and will often (but not always) be discharged and turned into foralls
        -- before reaching @qualifyType@. User written effect variables will still be returned in the second sequence.
        --                          μ         ϕ
        go :: UType -> Sem r (Type, Seq TVar, Seq TVar)
        go (UTCon tyName) = (\(tyName', k, _, _) -> (TCon tyName' k, Empty, Empty)) <$> lookupType tyName
        go (UTApp f x) = do
            (f', effs1, tvs1) <- go f
            (x', effs2, tvs2) <- go x
            pure (TApp f' x', effs1 <> effs2, tvs1 <> tvs2)
        go (UTFun a effs b) = do
            (a', aEffs, aVars) <- go a
            (effs', effEffs, effVars) <- go effs
            (b', bEffs, bVars) <- go b
            pure (TFun (foldr TForall a' aEffs) effs' b', effEffs <> bEffs, aVars <> effVars <> bVars)
        go (UTTyVar tv) = runError (lookupTVar tv) >>= \case
            Right tv' -> do
                traceM TraceQualify $ "[qualifyType (" <> show uty <> ")]: " <> show tv <> " ==> " <> show tv'
                pure (TTyVar tv', Empty, Empty)
            Left err -> do
                    tv' <- qualifyTVar tv Nothing
                    addTVar tv tv'
                    pure (TTyVar tv', [], [tv'])
        go (UTForall ps ty) = do
            ps' <- traverse (uncurry qualifyTVar) ps
            zipWithM_ addTVar (map fst ps) ps'
            (ty', tyEffs, tyVars) <- go ty
            pure (foldr TForall ty' ps', tyEffs, tyVars)
        go (UTConstraint constr ty) = do
            (constr', conEffs, conVars) <- goConstraint constr
            (ty', tyEffs, tyVars) <- go ty
            pure (TConstraint constr' ty', conEffs <> tyEffs, conVars <> tyVars)

        go (UTRowClosed Empty) = do
            tvar <- MkTVar <$> freshVar "μ" <*> pure (KRow KEffect)
            pure (TTyVar tvar, [tvar], Empty)

        go (UTRowClosed tys) = do
            -- TODO: We currently open *all* closed rows. If we ever use row polymorphism
            -- for anything other than effects (e.g. for extensible records), we have to make sure 
            -- to only do this if the row is part of a function type.
            tvar <- MkTVar <$> freshVar "μ" <*> pure (KRow KEffect)
            (tys', tyEffs, tyVars) <- goAll tys
            
            pure (TRowVar tys' tvar, (tvar :<| tyEffs), tyVars)
        go (UTRowVar tys var) = do
            (tys', tyEffs, tyVars) <- goAll tys
            runError (lookupTVar var) >>= \case 
                Right tv' -> pure (TRowVar tys' tv', tyEffs, tyVars)
                Left err -> do
                        tv' <- qualifyTVar var (Just (KRow KEffect)) -- TODO: Do we want to hardcode effect kinds here?
                        addTVar var tv'
                        pure (TRowVar tys' tv', tyEffs, tyVars |> tv')

        goAll :: Seq UType -> Sem r (Seq Type, Seq TVar, Seq TVar)
        goAll tys = do
            tys' :: Seq (Type, Seq TVar, Seq TVar) <- traverse go tys 
            let (tys'' :: Seq Type, effVars :: Seq (Seq TVar, Seq TVar)) = unzipWith (\(ty, effs, vars) -> (ty, (effs, vars))) tys'
                (effs, vars) = bimap fold fold (unzip effVars)
            pure (tys'', effs, vars)

        goConstraint :: UConstraint -> Sem r (Constraint, Seq TVar, Seq TVar)
        goConstraint (MkUConstraint className ty) = lookupType className >>= \case
            (className', k, TyClass _ _, _) -> do
                (ty', tyEffs, tyVars) <- go ty
                pure (MkConstraint className' k ty', tyEffs, tyVars)
            (className', k, tv, _) -> ask >>= \li -> throw $ NonClassInConstraint li className' k tv


qualifyTVar :: Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError, Reader LexInfo] r 
            => UnqualifiedName
            -> Maybe Kind
            -> Sem r TVar
qualifyTVar n mk = do
    li <- ask
    MkTVar <$> freshVar n <*> pure (fromMaybe KStar mk)

freshGlobal :: Members '[Reader (Tagged "ModName" Text)] r => Text -> Sem r QualifiedName
freshGlobal name = asks unTagged <&> \modName -> UnsafeQualifiedName name (GlobalQName modName)

addVar :: Members '[StackState Scope] r => Text -> QualifiedName -> Sem r ()
addVar n n' = smodify (scopeVars %~ insert n n')

addFixity :: Members '[StackState Scope] r => Text -> Fixity -> Sem r ()
addFixity n f = smodify (scopeFixities %~ insert n f)

addMFixity :: Members '[StackState Scope] r => Text -> Maybe Fixity -> Sem r ()
addMFixity n (Just f) = addFixity n f
addMFixity _ Nothing = pure ()

addVariantConstr :: Members '[StackState Scope] r => Text -> QualifiedName -> Int -> Int -> TypeVariant -> Sem r ()
addVariantConstr n n' ep i v = smodify (scopeVariantConstrs %~ insert n (n', ep, i, v))

addType :: Members '[StackState Scope] r => Text -> QualifiedName -> Kind -> TypeVariant -> Sem r ()
addType n n' k tv = smodify (scopeTypes %~ insert n (n', k, tv, False))

addTVar :: Members '[StackState Scope] r => UnqualifiedName -> TVar -> Sem r ()
addTVar n (MkTVar n' k) = smodify (scopeTVars %~ insert n (n', k))

addEffOp :: Members '[StackState Scope] r => Text -> QualifiedName -> Sem r ()
addEffOp n n' = smodify (scopeEffOperations %~ insert n n')

lookupVar :: Members '[StackState Scope, Error QualificationError, Reader LexInfo] r => Text -> Sem r QualifiedName
lookupVar n = sgets (lookup n . _scopeVars) >>= \case
    Just n' -> pure n'
    Nothing -> ask >>= \li -> throw $ VarNotFound li n

lookupFixity :: Members '[StackState Scope, Error QualificationError, Reader LexInfo] r => Text -> Sem r Fixity
lookupFixity n = sgets (lookup n . _scopeFixities) >>= \case
    Just f -> pure f
    Nothing -> ask >>= \li -> throw $ FixityNotFound li n

lookupVariantConstr :: Members '[StackState Scope, Error QualificationError, Reader LexInfo] r => Text -> Sem r (QualifiedName, Int, Int, TypeVariant)
lookupVariantConstr n = sgets (lookup n . _scopeVariantConstrs) >>= \case
    Just c -> pure c
    Nothing -> ask >>= \li -> throw $ VariantConstrNotFound li n

lookupType :: Members '[StackState Scope, Error QualificationError, Reader LexInfo] r => Text -> Sem r (QualifiedName, Kind, TypeVariant, Bool)
lookupType n = sgets (lookup n . _scopeTypes) >>= \case
    Just t -> pure t
    Nothing -> ask >>= \li -> throw $ TypeNotFound li n 

lookupTVar :: Members '[StackState Scope, Error QualificationError, Reader LexInfo] r => UnqualifiedName -> Sem r TVar
lookupTVar n = sgets (lookup n . _scopeTVars) >>= \case
    Just (n', k) -> pure (MkTVar n' k)
    Nothing -> ask >>= \li -> throw $ TVarNotFound li n

lookupEffOp :: Members '[StackState Scope, Error QualificationError, Reader LexInfo] r => UnqualifiedName -> Sem r QualifiedName
lookupEffOp n = sgets (lookup n . _scopeEffOperations) >>= \case
    Just n' -> pure n'
    Nothing -> ask >>= \li -> throw $ EffOpNotFound li n

getTyConKind :: Seq TVar -> Kind
getTyConKind = foldr (\(MkTVar _ k) r -> k `KFun` r) KStar 

forM2 :: (Applicative f, ListLike l, Traversable l) => l a -> l b -> (a -> b -> f c) -> f (l c)
forM2 xs ys f = zipWithM f xs ys

{-#LANGUAGE TemplateHaskell#-}
module Cobble.Qualifier where

import Cobble.Prelude
import Cobble.Types

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
}

instance Semigroup Scope where
    s1 <> s2 = Scope {
        _scopeVars = _scopeVars s1 <> _scopeVars s2
    ,   _scopeTypes = _scopeTypes s1 <> _scopeTypes s2
    ,   _scopeTVars = _scopeTVars s1 <> _scopeTVars s2
    ,   _scopeVariantConstrs = _scopeVariantConstrs s1 <> _scopeVariantConstrs s2
    ,   _scopeFixities = _scopeFixities s1 <> _scopeFixities s2
    }

instance Monoid Scope where
    mempty = Scope mempty mempty mempty mempty mempty

makeLenses ''Scope

qualify :: Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError] r 
        => Module QualifyNames 
        -> Sem r (Module NextPass)
qualify (Module deps name stmnts) = runReader (MkTagged name) $ Module deps name <$> traverse qualifyStmnt stmnts

qualifyStmnt :: Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError, Reader (Tagged "ModName" Text)] r 
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

qualifyStmnt (DefClass () li n tvs meths) = runReader li $ do
    n' <- freshGlobal n
    tvs' <- traverse (uncurry qualifyTVar) tvs
    let k = foldr (\(MkTVar _ k) r -> k `KFun` r) KConstraint tvs'
    -- Again, the methods have to be stubbed out since we don't have them yet, but we
    -- need the tyclass type in order to qualify them
    meths' <- withFrame $ do
        addType n n' k (TyClass (coercePass tvs') [])
        zipWithM_ addTVar (map fst tvs) tvs'
        forM meths \(mn, mty) -> do
            mn' <- freshGlobal mn
            mt' <- qualifyType True mty -- Type class methods *are* allowed to introduce new tyvars
            pure (mn', mt')
    addType n n' k (TyClass tvs' meths')
    zipWithM_ (\(methName,_) (methName',_) -> addVar methName methName') meths meths'
    pure (DefClass k li n' tvs' meths')

qualifyStmnt (DefInstance () li cname ty meths) = runReader li $ lookupType cname >>= \case
    (cname', k, TyClass tvs classMeths, isImported) -> withFrame do
        ty' <- qualifyType True ty 
        meths' <- forM meths \d@(Decl _ n _ _) -> do
            n' <- lookupVar n
            qualifyDeclWithName n' d
        pure (DefInstance (k, classMeths, tvs, isImported) li cname' ty' meths')
    (cname', k, tv, _) -> throw $ InstanceForNonClass li cname' k tv

-- | Same as qualifyDecl, but takes the already qualified name as an argument
-- instead of recomputing it
qualifyDeclWithName :: Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError, Reader LexInfo] r
            => QualifiedName
            -> Decl 'QualifyNames 
            -> Sem r (Decl NextPass)
qualifyDeclWithName n' (Decl () _ ps e) = withFrame do
    li <- ask
    ps' <- traverse freshVar ps
    zipWithM_ addVar ps ps'
    e' <- qualifyExpr e
    pure (Decl () n' ps' e')

qualifyRecursiveLocalDecl :: Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError, Reader LexInfo] r
                     => Decl 'QualifyNames 
                     -> Sem r (Decl NextPass)
qualifyRecursiveLocalDecl d@(Decl _ n _ _) = do
    li <- ask
    n' <- freshVar n
    addVar n n'
    qualifyDeclWithName n' d

qualifyExpr :: Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError] r
            => Expr QualifyNames
            -> Sem r (Expr NextPass)
qualifyExpr (App () li f e) =
    App () li
    <$> qualifyExpr f
    <*> qualifyExpr e
qualifyExpr (IntLit () li n) = pure (IntLit () li n)
qualifyExpr (UnitLit li) = pure (UnitLit li)
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

qualifyExpr (ExprX opGroup li) = runReader li $ replaceOpGroup . reorderByFixity <$> qualifyWithFixity opGroup
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

qualifyCaseBranch :: Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError] r
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

qualifyType :: forall r. Members '[StackState Scope, Fresh Text QualifiedName, Error QualificationError, Reader LexInfo] r 
            => Bool
            -> UType
            -> Sem r Type
qualifyType allowFreeVars = go
    where
        go :: UType -> Sem r Type
        go (UTCon tyName) = (\(tyName', k, _, _) -> TCon tyName' k) <$> lookupType tyName
        go (UTApp f x) = TApp <$> go f <*> go x
        go (UTFun a b) = TFun <$> go a <*> go b
        go (UTVar tv) = runError (lookupTVar tv) >>= \case
            Right tv' -> pure (TVar tv')
            Left err
                | allowFreeVars -> do
                    tv' <- qualifyTVar tv Nothing
                    addTVar tv tv'
                    pure (TVar tv')
                | otherwise -> throw err
        go (UTForall ps ty) = withFrame do
            ps' <- traverse (uncurry qualifyTVar) ps
            zipWithM_ addTVar (map fst ps) ps'
            TForall ps' <$> go ty
        go (UTConstraint constr ty) = TConstraint <$> goConstraint constr <*> go ty

        goConstraint :: UConstraint -> Sem r Constraint
        goConstraint (MkUConstraint className ty) = lookupType className >>= \case
            (className', k, TyClass _ _, _) -> MkConstraint className' k <$> go ty
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

getTyConKind :: Seq TVar -> Kind
getTyConKind = foldr (\(MkTVar _ k) r -> k `KFun` r) KStar 


forM2 :: (Applicative f, ListLike l, Traversable l) => l a -> l b -> (a -> b -> f c) -> f (l c)
forM2 xs ys f = zipWithM f xs ys

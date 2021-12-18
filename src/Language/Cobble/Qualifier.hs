{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Qualifier where

import Language.Cobble.Prelude
import Language.Cobble.Types

import Language.Cobble.Util.Polysemy.StackState
import Language.Cobble.Util.Polysemy.Fresh
import Language.Cobble.Util.Bitraversable

type NextPass = SemAnalysis

data QualificationError = VarNotFound LexInfo Text
                        | FixityNotFound LexInfo Text
                        | VariantConstrNotFound LexInfo Text
                        | TypeNotFound LexInfo Text
                        | TVarNotFound LexInfo (TVar QualifyNames)
                        | InstanceForNonClass LexInfo QualifiedName Kind TypeVariant
                        | NonClassInConstraint LexInfo QualifiedName Kind TypeVariant
                        | StructConstructNotAStruct LexInfo QualifiedName Kind TypeVariant
                        deriving (Show, Eq)

data Scope = Scope {
    _scopeVars :: Map Text QualifiedName
,   _scopeTypes :: Map Text (QualifiedName, Kind, TypeVariant)
,   _scopeTVars :: Map Text (QualifiedName, Kind)
,   _scopeVariantConstrs :: Map Text (QualifiedName, Int, Int)
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

qualify :: Members '[StackState Scope, Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r 
        => Module QualifyNames 
        -> Sem r (Module NextPass)
qualify (Module deps name stmnts) = Module deps (internalQName name) <$> traverse qualifyStmnt stmnts

qualifyStmnt :: Members '[StackState Scope, Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r 
             => Statement QualifyNames
             -> Sem r (Statement NextPass)
qualifyStmnt (Def mfixity li d@(Decl _ n _ _) ty) = runReader li do 
    n' <- freshVar (n, li)
    addVar n n'
    addMFixity n mfixity
    withFrame do
        -- The type has to be qualified first, since all mentioned tyvars
        -- need to be in scope in the body (mostly for ascriptions).
        ty' <- qualifyType True ty
        Def () li
            <$> qualifyDeclWithName n' d
            <*> pure ty'
qualifyStmnt (Import () li n) = pure (Import () li (internalQName n))

qualifyStmnt (DefStruct () li n tvs fields) = runReader li do
    n' <- freshVar (n, li)
    tvs' <- traverse (qualifyTVar KStar) tvs -- TODO: [Kind inference]: probably shouldn't be KStar
    let k = getTyConKind tvs'
    fields' <- withFrame do
        -- The type needs to be locally added as a dummy record to allow recursive types.
        addType n n' k (RecordType (coercePass tvs') [])
        zipWithM_ addTVar tvs tvs'

        forM fields \(fn, fty) -> withFrame do
            (fn,) <$> qualifyType False fty
    addType n n' k (RecordType (coercePass tvs') (map (second coercePass) fields'))
    pure (DefStruct k li n' tvs' fields')

qualifyStmnt (DefVariant () li n tvs constrs) = runReader li $ do
    n' <- freshVar (n, li)
    tvs' <- traverse (qualifyTVar KStar) tvs -- TODO: [Kind inference]: probably shouldn't be KStar
    let k = getTyConKind tvs'
    constrs' <- withFrame $ forM2 [0..] constrs \i (cn, tys, ()) -> do
        -- The type needs to be locally added as a dummy variant to allow recursive types.
        addType n n' k (VariantType (coercePass tvs') [])
        zipWithM_ addTVar tvs tvs'

        cn' <- freshVar (cn, li)
        tys' <- traverse (qualifyType False) tys
        pure (cn', tys', (length tys', i))
    addType n n' k (VariantType (coercePass tvs') (map (\(cn, ts, _) -> (cn, coercePass ts)) constrs'))
    zipWithM_ (\(cn, _, _) (cn', _, (ep, i)) -> addVariantConstr cn cn' ep i) constrs constrs'
    pure (DefVariant k li n' tvs' constrs')

qualifyStmnt (DefClass () li n tvs meths) = runReader li $ do
    n' <- freshVar (n, li)
    tvs' <- traverse (qualifyTVar KStar) tvs -- TODO: [Kind inference]: probably shouldn't be KStar
    let k = foldr (\(MkTVar _ k) r -> k `KFun` r) KConstraint tvs'
    -- Again, the methods have to be stubbed out since we don't have them yet, but we
    -- need the tyclass type in order to qualify them
    meths' <- withFrame $ do
        addType n n' k (TyClass (coercePass tvs') [])
        zipWithM_ addTVar tvs tvs'
        forM meths \(mn, mty) -> do
            mn' <- freshVar (mn, li)
            mt' <- qualifyType True mty -- Type class methods *are* allowed to introduce new tyvars
            pure (mn', mt')
    addType n n' k (TyClass (coercePass tvs') (map (second coercePass) meths'))
    zipWithM_ (\(methName,_) (methName',_) -> addVar methName methName') meths meths'
    pure (DefClass k li n' tvs' meths')

qualifyStmnt (DefInstance () li cname ty meths) = runReader li $ lookupType cname >>= \case
    (cname', _, TyClass tvs classMeths) -> withFrame do
        ty' <- qualifyType False ty 
        -- TODO: Should ty vars in classes be allowed? They are in Haskell, but I'm not sure if cobble is quite
        -- ready to work with these kinds of instances
        meths' <- forM meths \d@(Decl _ n _ _) -> do
            n' <- lookupVar n
            qualifyDeclWithName n' d
        pure (DefInstance (classMeths, tvs) li cname' ty' meths')
    (cname', k, tv) -> throw $ InstanceForNonClass li cname' k tv

-- | Same as qualifyDecl, but takes the already qualified name as an argument
-- instead of recomputing it
qualifyDeclWithName :: Members '[StackState Scope, Fresh (Text, LexInfo) QualifiedName, Error QualificationError, Reader LexInfo] r
            => QualifiedName
            -> Decl 'QualifyNames 
            -> Sem r (Decl NextPass)
qualifyDeclWithName n' (Decl () _ ps e) = withFrame do
    li <- ask
    ps' <- traverse (freshVar . (,li)) ps
    zipWithM_ addVar ps ps'
    e' <- qualifyExpr e
    pure (Decl () n' ps' e')

qualifyDecl :: Members '[StackState Scope, Fresh (Text, LexInfo) QualifiedName, Error QualificationError, Reader LexInfo] r
            => Decl 'QualifyNames 
            -> Sem r (Decl NextPass)
qualifyDecl d@(Decl _ n _ _) = do
    li <- ask
    n' <- freshVar (n, li)
    qualifyDeclWithName n' d
        <* addVar n n'

qualifyRecursiveDecl :: Members '[StackState Scope, Fresh (Text, LexInfo) QualifiedName, Error QualificationError, Reader LexInfo] r
                     => Decl 'QualifyNames 
                     -> Sem r (Decl NextPass)
qualifyRecursiveDecl d@(Decl _ n _ _) = do
    li <- ask
    n' <- freshVar (n, li)
    addVar n n'
    qualifyDeclWithName n' d

qualifyExpr :: Members '[StackState Scope, Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r
            => Expr QualifyNames
            -> Sem r (Expr NextPass)
qualifyExpr (FCall () li f args) =
    FCall () li
    <$> qualifyExpr f
    <*> traverse qualifyExpr args
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
        <$> qualifyRecursiveDecl decl
        <*> qualifyExpr b
qualifyExpr (Var () li x) = runReader li $ Var () li <$> lookupVar x
qualifyExpr (Ascription () li expr ty) = runReader li $
    Ascription () li 
    <$> qualifyExpr expr
    <*> qualifyType False ty
qualifyExpr (VariantConstr () li cname) = runReader li do
    (cname', ep, i) <- lookupVariantConstr cname
    pure $ VariantConstr (ep, i) li cname'
qualifyExpr (Case () li exp cases) = runReader li $ 
    Case () li
    <$> qualifyExpr exp
    <*> traverse qualifyCaseBranch cases
qualifyExpr (StructConstruct () li sname fields) = runReader li $ lookupType sname >>= \case
    (sname', _, RecordType tvs structFields) -> 
        StructConstruct 
            (StructDef sname' (coercePass tvs) (map (second coercePass) structFields))
            li 
            sname' 
            <$> traverse (secondM qualifyExpr) fields
    (tname', k, v) -> throw (StructConstructNotAStruct li tname' k v)
qualifyExpr (StructAccess () li structExpr fieldName) = runReader li do
    allStructs <- sgets (toList . view scopeTypes)
    let possibleStructs = fromList $ mapMaybe possibleStruct allStructs
    StructAccess possibleStructs li
        <$> qualifyExpr structExpr
        <*> pure fieldName
        where
            possibleStruct :: (QualifiedName, Kind, TypeVariant) -> Maybe (QualifiedName, StructDef NextPass)
            possibleStruct (tyName, _, (RecordType tvs fields))
                | fieldName `elem` (map fst fields) = Just (tyName, StructDef tyName (coercePass tvs) (map (second coercePass) fields))
            possibleStruct _ = Nothing
qualifyExpr (ExprX opGroup li) = runReader li $ replaceOpGroup . reorderByFixity <$> qualifyWithFixity opGroup
    where
        qualifyWithFixity :: Members '[StackState Scope, Fresh (Text, LexInfo) QualifiedName, Error QualificationError, Reader LexInfo] r
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
        replaceOpGroup (OpNode l (op, _fixity) r) = FCall () li (Var () li op) (fromList [
                                                            replaceOpGroup l
                                                        ,   replaceOpGroup r
                                                        ])
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

qualifyCaseBranch :: Members '[StackState Scope, Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r
                  => CaseBranch QualifyNames
                  -> Sem r (CaseBranch NextPass)
qualifyCaseBranch (CaseBranch () li pat expr) = runReader li $
    withFrame $ CaseBranch () li
        <$> qualifyPattern pat
        <*> qualifyExpr expr

qualifyPattern :: Members '[StackState Scope, Fresh (Text, LexInfo) QualifiedName, Error QualificationError, Reader LexInfo] r
               => Pattern QualifyNames
               -> Sem r (Pattern NextPass)
qualifyPattern (IntP () n) = pure (IntP () n)
qualifyPattern (VarP () x) = do
    li <- ask
    x' <- freshVar (x, li)
    addVar x x'
    pure (VarP () x')
qualifyPattern (ConstrP () cname ps) = do
    (cname, _, i) <- lookupVariantConstr cname
    ConstrP i cname <$> traverse qualifyPattern ps

qualifyType :: forall r. Members '[StackState Scope, Fresh (Text, LexInfo) QualifiedName, Error QualificationError, Reader LexInfo] r 
            => Bool
            -> Type QualifyNames
            -> Sem r (Type NextPass)
qualifyType allowFreeVars = go
    where
        go :: Type QualifyNames -> Sem r (Type NextPass)
        go (TCon tyName ()) = (\(tyName', k, _) -> TCon tyName' k) <$> lookupType tyName
        go (TApp f x) = TApp <$> go f <*> go x
        go (TVar tv) = runError (lookupTVar tv) >>= \case
            Right tv' -> pure (TVar tv')
            Left err
                | allowFreeVars -> do
                    tv' <- qualifyTVar KStar tv -- TODO: [Kind inference]
                    addTVar tv tv'
                    pure (TVar tv')
                | otherwise -> throw err
        go (TSkol tv) = error $ "Source level skolems should not exist: " <> show (TSkol tv)
        go (TForall ps ty) = withFrame do
            ps' <- traverse (qualifyTVar KStar) ps -- TODO: [Kind inference]: should not just be KStar
            zipWithM_ addTVar ps ps'
            TForall ps' <$> go ty
        go (TConstraint constr ty) = TConstraint <$> goConstraint constr <*> go ty

        goConstraint :: Constraint QualifyNames -> Sem r (Constraint NextPass)
        goConstraint (MkConstraint className ty) = lookupType className >>= \case
            (className', _, TyClass _ _) -> MkConstraint className' <$> go ty
            (className', k, tv) -> ask >>= \li -> throw $ NonClassInConstraint li className' k tv


qualifyTVar :: Members '[StackState Scope, Fresh (Text, LexInfo) QualifiedName, Error QualificationError, Reader LexInfo] r 
            => Kind
            -> TVar QualifyNames
            -> Sem r (TVar NextPass)
qualifyTVar k (MkTVar n ()) = ask >>= \li -> MkTVar <$> freshVar (n, li) <*> pure k

addVar :: Members '[StackState Scope] r => Text -> QualifiedName -> Sem r ()
addVar n n' = smodify (scopeVars %~ insert n n')

addFixity :: Members '[StackState Scope] r => Text -> Fixity -> Sem r ()
addFixity n f = smodify (scopeFixities %~ insert n f)

addMFixity :: Members '[StackState Scope] r => Text -> Maybe Fixity -> Sem r ()
addMFixity n (Just f) = addFixity n f
addMFixity _ Nothing = pure ()

addVariantConstr :: Members '[StackState Scope] r => Text -> QualifiedName -> Int -> Int -> Sem r ()
addVariantConstr n n' ep i = smodify (scopeVariantConstrs %~ insert n (n', ep, i))

addType :: Members '[StackState Scope] r => Text -> QualifiedName -> Kind -> TypeVariant -> Sem r ()
addType n n' k tv = smodify (scopeTypes %~ insert n (n', k, tv))

addTVar :: Members '[StackState Scope] r => TVar QualifyNames -> TVar NextPass -> Sem r ()
addTVar (MkTVar n ()) (MkTVar n' k) = smodify (scopeTVars %~ insert n (n', k))

lookupVar :: Members '[StackState Scope, Error QualificationError, Reader LexInfo] r => Text -> Sem r QualifiedName
lookupVar n = sgets (lookup n . _scopeVars) >>= \case
    Just n' -> pure n'
    Nothing -> ask >>= \li -> throw $ VarNotFound li n

lookupFixity :: Members '[StackState Scope, Error QualificationError, Reader LexInfo] r => Text -> Sem r Fixity
lookupFixity n = sgets (lookup n . _scopeFixities) >>= \case
    Just f -> pure f
    Nothing -> ask >>= \li -> throw $ FixityNotFound li n

lookupVariantConstr :: Members '[StackState Scope, Error QualificationError, Reader LexInfo] r => Text -> Sem r (QualifiedName, Int, Int)
lookupVariantConstr n = sgets (lookup n . _scopeVariantConstrs) >>= \case
    Just c -> pure c
    Nothing -> ask >>= \li -> throw $ VariantConstrNotFound li n

lookupType :: Members '[StackState Scope, Error QualificationError, Reader LexInfo] r => Text -> Sem r (QualifiedName, Kind, TypeVariant)
lookupType n = sgets (lookup n . _scopeTypes) >>= \case
    Just t -> pure t
    Nothing -> ask >>= \li -> throw $ TypeNotFound li n 

lookupTVar :: Members '[StackState Scope, Error QualificationError, Reader LexInfo] r => TVar QualifyNames -> Sem r (TVar NextPass)
lookupTVar (MkTVar n ()) = sgets (lookup n . _scopeTVars) >>= \case
    Just (n', k) -> pure (MkTVar n' k)
    Nothing -> ask >>= \li -> throw $ TVarNotFound li (MkTVar n ())

getTyConKind :: [TVar NextPass] -> Kind
getTyConKind = foldr (\(MkTVar _ k) r -> k `KFun` r) KStar 


forM2 :: Applicative f => [a] -> [b] -> (a -> b -> f c) -> f [c]
forM2 xs ys f = zipWithM f xs ys

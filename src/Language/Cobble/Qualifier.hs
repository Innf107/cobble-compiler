{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Qualifier where

import Language.Cobble.Prelude
import Language.Cobble.Types
import Language.Cobble.Types.Lens

import Language.Cobble.Util
import Language.Cobble.Util.Bitraversable
import Language.Cobble.Util.Polysemy.Fresh

import Data.Map qualified as M

type NextPass = SemAnalysis
  
data QualificationError = NameNotFound LexInfo Text
                        | VariantConstrNotFound LexInfo Text
                        | TypeNotFound LexInfo Text
                        | TVarNotFound LexInfo Text
                        | FixityNotFound LexInfo Text
                        | NotAStruct LexInfo QualifiedName Kind TypeVariant
                        | VarAlreadyDeclaredInScope LexInfo Text
                        | VariantConstrAlreadyDeclaredInScope LexInfo Text
                        | TypeAlreadyDeclaredInScope LexInfo Text
                        | TVarAlreadyDeclaredInScope LexInfo Text
                        | AmbiguousVarName LexInfo Text [QualifiedName]
                        | AmbiguousVariantConstrName LexInfo Text [(QualifiedName, Int)]
                        | AmbiguousTypeName LexInfo Text [(QualifiedName, Kind, TypeVariant)]
                        | AmbiguousTVarName LexInfo Text [(QualifiedName, Kind)]
                        | InstanceForNonClass LexInfo QualifiedName Kind TypeVariant
                        | NonClassInConstraint LexInfo QualifiedName Kind TypeVariant
                        deriving (Show, Eq)

data Scope = Scope {
        _scopeVars :: Map Text QualifiedName
    ,   _scopeTypes :: Map Text (QualifiedName, Kind, TypeVariant)
    ,   _scopeTVars :: Map Text (QualifiedName, Kind)
    ,   _scopeVariantConstrs :: Map Text (QualifiedName, Int, Int)
    ,   _scopeFixities :: Map Text Fixity
    } deriving (Show, Eq)

makeLenses ''Scope

lookupVar :: Members '[Reader [Scope], Error QualificationError] r => LexInfo -> Text -> Sem r QualifiedName 
lookupVar l n = do
    scopes <- ask
    case mapMaybe (lookup n . view scopeVars) scopes of 
        []  -> throw $ NameNotFound l n
        [x] -> pure x
        xs  -> throw $ AmbiguousVarName l n xs

lookupVariantConstr :: Members '[Reader [Scope], Error QualificationError] r => LexInfo -> Text -> Sem r (QualifiedName, Int, Int) 
lookupVariantConstr l n = do
    scopes <- ask
    case mapMaybe (lookup n . view scopeVariantConstrs) scopes of
        []  -> throw $ VariantConstrNotFound l n
        [x] -> pure x
        xs  -> throw $ AmbiguousVariantConstrName l n (map (\(qn, eps, _) -> (qn,eps)) xs)

lookupType :: Members '[Reader [Scope], Error QualificationError] r 
           => LexInfo 
           -> UnqualifiedName  
           -> Sem r (QualifiedName, Kind, TypeVariant) 
lookupType l n = do
    scopes <- ask
    case mapMaybe (lookup n . view scopeTypes) scopes of 
        []  -> throw $ TypeNotFound l n
        [x] -> pure x
        xs  -> throw $ AmbiguousTypeName l n xs

lookupTVar :: Members '[Reader [Scope], Error QualificationError] r 
           => LexInfo 
           -> Text 
           -> Sem r (QualifiedName, Kind)
lookupTVar l n = do
    scopes <- ask
    case mapMaybe (lookup n . view scopeTVars) scopes of 
        []  -> throw $ TVarNotFound l n
        [x] -> pure x
        xs  -> throw $ AmbiguousTVarName l n xs

lookupFixity :: Members '[Reader [Scope], Error QualificationError] r 
             => LexInfo 
             -> Text 
             -> Sem r Fixity 
lookupFixity l n = do
    scopes <- ask
    case mapMaybe (lookup n . view scopeFixities) scopes of 
        []  -> throw $ FixityNotFound l n
        [x] -> pure x
        xs  -> error $ "lookupFixity: multiple fixities for operator: " <> n <> "\n    fixities: " <> show xs

withVar :: Members '[Reader [Scope], Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r 
        => LexInfo 
        -> Text 
        -> (QualifiedName -> Sem r a) 
        -> Sem r a
withVar l n a = do
        n' <- freshVar (n, l)
        withVar' l n n' (a n')

withVar' :: Members '[Reader [Scope], Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r 
        => LexInfo 
        -> Text 
        -> QualifiedName
        -> Sem r a
        -> Sem r a
withVar' l n qn a = do
    alreadyInScope <- asks (member n .view (_head . scopeVars))
    if alreadyInScope 
    then throw (VarAlreadyDeclaredInScope l n)
    else local (_head . scopeVars %~ insert n qn) a

withVars :: Members '[Reader [Scope], Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r
    => LexInfo
    -> [UnqualifiedName]
    -> ([QualifiedName] -> Sem r a)
    -> Sem r a
withVars l ns a = do
    ns' <- traverse (\n -> freshVar (n, l)) ns
    withVars' l (zip ns ns') (a ns')

withVars' :: Members '[Reader [Scope], Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r  
        => LexInfo 
        -> [(Text, QualifiedName)]
        -> Sem r a
        -> Sem r a
withVars' = withMany (\l (x, x') -> withVar' l x x')

withMany :: (LexInfo -> a -> Sem r b -> Sem r b) 
          -> LexInfo
          -> [a] 
          -> Sem r b
          -> Sem r b
withMany f l xs a = do
    foldr (\x r -> f l x r) a xs

withVarAndFixity :: Members '[Reader [Scope], Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r 
        => Fixity
        -> LexInfo 
        -> Text 
        -> (QualifiedName -> Sem r a) 
        -> Sem r a
withVarAndFixity f l n a = do
                n' <- freshVar (n, l)
                alreadyInScope <- asks (member n .view (_head . scopeVars))
                if alreadyInScope 
                then throw (VarAlreadyDeclaredInScope l n)
                else local ((_head . scopeVars %~ insert n n')
                        .   (_head . scopeFixities %~ insert n f)) 
                        (a n')

withVariantConstr :: Members '[Reader [Scope], State Int, Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r 
        => LexInfo 
        -> Text 
        -> Int
        -> (QualifiedName -> Int -> Sem r a) 
        -> Sem r a
withVariantConstr l n ep a = do
        n' <- freshVar (n, l)
        ix <- state (\i -> (i, i+1))
        withVariantConstr' l n ep ix n' (a n' ix)



withVariantConstr' :: Members '[Reader [Scope], Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r 
        => LexInfo 
        -> Text
        -> Int 
        -> Int
        -> QualifiedName
        -> Sem r a 
        -> Sem r a
withVariantConstr' l n ep i qn a = do
    alreadyInScope <- asks (member n .view (_head . scopeVariantConstrs))
    if alreadyInScope 
    then throw (VariantConstrAlreadyDeclaredInScope l n)
    else local (_head . scopeVariantConstrs %~ insert n (qn, ep, i)) a

withVariantConstrs' :: Members '[Reader [Scope], Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r 
        => LexInfo 
        -> [(Text, Int, Int, QualifiedName)]
        -> Sem r a 
        -> Sem r a
withVariantConstrs' = withMany (\l (x,ep,i,x') -> withVariantConstr' l x ep i x')
    

withType :: Members '[Reader [Scope], Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r 
         => LexInfo
         -> UnqualifiedName 
         -> Kind
         -> TypeVariant
         -> (QualifiedName -> Sem r a) 
         -> Sem r a
withType l n k tv a = do
    n' <- freshVar (n, l)
    withType' l n n' k tv (a n')

withType' :: Members '[Reader [Scope], Error QualificationError] r 
         => LexInfo
         -> UnqualifiedName
         -> QualifiedName 
         -> Kind
         -> TypeVariant
         -> Sem r a
         -> Sem r a
withType' l n n' k tv a = do
    alreadyInScope <- asks (member n . view (_head . scopeTypes))
    if alreadyInScope 
    then throw (TypeAlreadyDeclaredInScope l n)
    else local (_head . scopeTypes %~ insert n (n', k, tv)) a

withTVars :: forall r a. Members '[Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
          => LexInfo
          -> [TVar QualifyNames]
          -> ([TVar NextPass] -> Sem r a)
          -> Sem r a
withTVars li tvs a = do
    tvs' <- zipWithM freshTVar tvs (repeat li)
    a tvs'
        where
            freshTVar :: TVar QualifyNames -> LexInfo -> Sem r (TVar NextPass)
            freshTVar (MkTVar name ()) li = MkTVar <$> freshVar (name, li) <*> pure KStar -- TODO
            
getConstrKind :: [TVar NextPass] -> Kind
getConstrKind = foldr (\(MkTVar _ k) r -> k `KFun` r) KStar 

qualify :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
        => Module QualifyNames
        -> Sem r (Module NextPass)
qualify (Module (Ext deps) name stmnts) = Module (Ext deps) (unsafeQualifiedName name name (LexInfo (SourcePos 0 0) (SourcePos 0 0) name)) 
                                       <$>qualifyStmnts stmnts

qualifyStmnts :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
             => [Statement QualifyNames]
             -> Sem r [Statement NextPass]
qualifyStmnts = \case
    [] -> pure []
    (Def (Ext mfixity) li decl@(Decl _ n _ _) ty : sts) -> (maybe withVar withVarAndFixity mfixity) li n $ \n' -> 
        (:)
        <$> (Def IgnoreExt li
                <$> qualifyDeclWith n' li decl
                <*> qualifyType li ty)
        <*> qualifyStmnts sts

    (Import IgnoreExt li m : sts) -> 
        (:)
        <$> pure (Import IgnoreExt li $ unsafeQualifiedName m m li)
        <*> qualifyStmnts sts

    (DefStruct IgnoreExt li n ps fields : sts) -> do
        (def, fields', n', k, ps') <- withTVars li ps $ \ps' -> 
            let k = getConstrKind ps' in
        --                  Ugly hack to allow recursive types :/
            withType li n k (RecordType (coercePass ps') []) $ \qn ->
                (\fs' -> (DefStruct (Ext k) li qn ps' fs', fs', qn, k, coercePass ps')) 
                <$> traverse (secondM (qualifyType li)) fields
        (def :)
                <$> withType' li n n' k (RecordType ps' (map (second coercePass) fields')) 
                    (qualifyStmnts sts)

    -- Type Constructors should `not` just be treated as functions, since that would
    -- massively complicate codegen
    (DefVariant IgnoreExt li n ps constrs : sts) -> do
        (def, k, n', ps', constrs') <- withTVars li ps \ps' -> 
            let k = getConstrKind ps' in
            --                 Same ugly hack :/
            withType li n k (VariantType (coercePass ps') []) \n' -> do
                constrs' <- evalState 0 $ forM constrs \(cname, ctys, IgnoreExt) -> do
                    ctys' <- traverse (qualifyType li) ctys
                    withVariantConstr li cname (length ctys) \cname' i -> 
                        pure (cname', ctys', length ctys, i)
                pure (DefVariant (Ext k) li n' ps' (map (\(x, y, ep, ix) -> (x,y, Ext (ep, ix))) constrs'), k, n', ps', constrs')
        (def :)
            <$> (withType' li n n' k (VariantType (coercePass ps') (map (\(x,y,_,_) -> (x, coercePass y)) constrs')) 
                $ withVariantConstrs' li (zipWith (\(x,_,_)(y,_,ep,i) -> (x, ep, i, y)) constrs constrs')
                $ qualifyStmnts sts)

    -- Typeclass definitions also need the ugly hack, because
    -- the renamed methods have to be inserted in @withType@...
    (DefClass IgnoreExt li n ps meths : sts) ->
        withTVars li ps \ps' -> do
            let k = foldr (\(MkTVar _ k) r -> k `KFun` r) KConstraint ps'
            (n', meths') <- withType li n k (TyClass (coercePass ps') []) \n' ->
                let (methNames, methTys) = unzip meths in
                withVars li methNames \methNames' -> do
                    (n',) . zip methNames' <$> (traverse (qualifyTypeWithTVars (fromList (zip ps ps')) li) methTys) 
            withType' li n n' k (TyClass (coercePass ps') (map (second coercePass) meths')) $
                withVars' li (map (\(qn,_) -> (originalName qn, qn)) meths') $
                    (:)
                        <$> pure (DefClass (Ext k) li n' ps' meths')
                        <*> qualifyStmnts sts
    (DefInstance IgnoreExt li cn t decls : sts) ->
        lookupType li cn >>= \case
            (cn', _, TyClass ps tys) -> do
                t' <- qualifyType li t
                (:)
                    <$> (DefInstance (Ext (tys, ps)) li cn' t' <$> forM decls \d -> qualifyExistingDecl li d)
                    <*> qualifyStmnts sts
            (cn', k, tv) -> throw (InstanceForNonClass li cn' k tv)

qualifyExp :: forall r. Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
           => Expr QualifyNames
           -> Sem r (Expr NextPass)
qualifyExp = \case
    FCall IgnoreExt li e args   -> FCall IgnoreExt li <$> qualifyExp e <*> traverse qualifyExp args
    IntLit IgnoreExt li n       -> pure $ IntLit IgnoreExt li n
    UnitLit li                  -> pure $ UnitLit li
    If IgnoreExt li cond th el  -> If IgnoreExt li <$> qualifyExp cond <*> qualifyExp th <*> qualifyExp el
    Let IgnoreExt li decl@(Decl _ n _ _) b  -> withVar li n $ \n' -> Let IgnoreExt li <$> qualifyDeclWith n' li decl <*> qualifyExp b
    Var IgnoreExt li n          -> Var IgnoreExt li <$> lookupVar li n
    VariantConstr IgnoreExt li n -> lookupVariantConstr li n <&> \(n', ep, i) -> VariantConstr (Ext (ep, i)) li n'
    StructConstruct IgnoreExt li structName fields -> lookupType li structName >>= \case
            (structName', _k, RecordType ps tyFields) -> traverse (secondM qualifyExp) fields <&> \fields' -> 
                StructConstruct (StructDef structName' (coercePass ps) (map (second coercePass) tyFields)) li structName' fields'
            (tyName', k, variant) -> throw (NotAStruct li tyName' k variant)
    StructAccess IgnoreExt li se f -> do
        allStructs <- asks (concatMap (toList . view scopeTypes))
        let possibleStructs = fromList $ allStructs & mapMaybe \(tyName, kind, tyVariant) -> case tyVariant of
                RecordType ps fields -> if f `elem` (map fst fields) 
                    then Just $ (tyName, StructDef tyName (coercePass ps) (map (second coercePass) fields))
                    else Nothing 
                _ -> Nothing
        StructAccess possibleStructs li 
            <$> qualifyExp se
            <*> pure f
    ExprX opGroup li -> replaceOpGroup . reorderByFixity <$> qualifyWithFixity opGroup
        where
            qualifyWithFixity :: OperatorGroup QualifyNames NoFixity -> Sem r (OperatorGroup NextPass WithFixity)
            qualifyWithFixity (OpLeaf e)            = OpLeaf <$> qualifyExp e
            qualifyWithFixity (OpNode l (op, ()) r) = do
                f <- lookupFixity li op
                op' <- lookupVar li op
                OpNode
                    <$> qualifyWithFixity l
                    <*> pure (op', f)
                    <*> qualifyWithFixity r
            replaceOpGroup :: OperatorGroup NextPass WithFixity -> Expr NextPass
            replaceOpGroup (OpLeaf e) = e
            replaceOpGroup (OpNode l (op, _fixity) r) = FCall IgnoreExt li (Var IgnoreExt li op) (fromList [
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


{-  Note [Fixity Algorithm]:
    Operator fixity is determined here in the qualifier, since that means that parsing is still context-free.
    Instead, multiple operators (without parentheses) are parsed right associatively and @reorderByFixity@
    reorders them according to the (possibly imported) fixity.

    Algorithm: Perform a left rotation in y and recursively repair the left subtree 
                iff precedence(x) < precedence(y) or precedence(x) = precedence(y) and associativity(x) = infixl
    Complexity: O(n²) :/ (Probably not that bad, since opGroups don't tend to be that large)
        x                                               y
       / \                                             / \
      1   y  --(precedence(x) < precedence(y))->      x  ...
         / \                                         / \
        2  ...                                      1   2
-}


{-  Takes a name and qualifys the @Decl@ accordingly, replacing the old name with the new one.
    No check is performed to verify that the names match.
    
    This should typically be used in conjunction with @withVar@, e.g.
    ```
    withVar li n >>= \n' -> qualifyDeclWith li n' decl
    ```
-}
qualifyDeclWith :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
                => QualifiedName
                -> LexInfo
                -> Decl QualifyNames
                -> Sem r (Decl NextPass)
qualifyDeclWith n' li (Decl IgnoreExt _ (Ext params) e) = 
    uncurry (Decl IgnoreExt n' . Ext)
    <$> foldr (\p r -> withVar li p \p' -> first (p' :) <$> r) (([],) <$> qualifyExp e) params

-- Qualifies a @Decl@ by looking up the name in the environment
-- This should be used, when the function name is already known, e.g. in type class instances  
qualifyExistingDecl :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
            => LexInfo
            -> Decl QualifyNames
            -> Sem r (Decl NextPass)
qualifyExistingDecl li d@(Decl _ n _ _) = lookupVar li n >>= \n' -> qualifyDeclWith n' li d

qualifyTypeWithTVars :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
            => Map (TVar QualifyNames) (TVar NextPass)
            -> LexInfo
            -> Type QualifyNames
            -> Sem r (Type NextPass)
qualifyTypeWithTVars tvs li = \case
    TCon n () -> (\(n', k, _) -> TCon n' k) <$> lookupType li n
    TVar tv 
        | Just tv' <- lookup tv tvs -> pure (TVar tv')
        | MkTVar n () <- tv -> pure $ TVar (MkTVar (unsafeQualifiedName n n li) KStar)
    TSkol (MkTVar _ ())     -> error "qualifyTypeWithTVars: source-level skolems should not exist"
    TApp t1 t2              -> TApp <$> qualifyTypeWithTVars tvs li t1 <*> qualifyTypeWithTVars tvs li t2 
    TForall _ _             -> error "source-level foralls NYI"
    TConstraint constr t    -> TConstraint <$> qualifyConstraintWithTVars tvs li constr <*> qualifyTypeWithTVars tvs li t
    where

qualifyType :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
            => LexInfo 
            -> Type QualifyNames
            -> Sem r (Type NextPass)
qualifyType = qualifyTypeWithTVars mempty

-- TODO: TyClass carries parameters now, so this should be unnecessary?
qualifyConstraintWithTVars :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
                  => Map (TVar QualifyNames) (TVar NextPass)
                  -> LexInfo
                  -> Constraint QualifyNames
                  -> Sem r (Constraint NextPass)
qualifyConstraintWithTVars tvs li  (MkConstraint className arg) = lookupType li className >>= \case
    (className', _, TyClass _ _) -> MkConstraint className' <$> qualifyTypeWithTVars tvs li arg
    (className', k, v) -> throw $ NonClassInConstraint li className' k v

qualifyConstraint :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
                  => LexInfo
                  -> Constraint QualifyNames
                  -> Sem r (Constraint NextPass)
qualifyConstraint = qualifyConstraintWithTVars mempty


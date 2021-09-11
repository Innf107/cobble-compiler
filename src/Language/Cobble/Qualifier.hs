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
  
--type QualifyC r = Members '[State Int, State [Scope], Error QualificationError, Output Log] r

data QualificationError = NameNotFound LexInfo Text
                        | TypeNotFound LexInfo Text
                        | TVarNotFound LexInfo Text
                        | FixityNotFound LexInfo Text
                        | NotAStruct LexInfo QualifiedName Kind TypeVariant
                        | VarAlreadyDeclaredInScope LexInfo Text
                        | TypeAlreadyDeclaredInScope LexInfo Text
                        | TVarAlreadyDeclaredInScope LexInfo Text
                        | AmbiguousVarName LexInfo Text [QualifiedName]
                        | AmbiguousTypeName LexInfo Text [(QualifiedName, Kind, TypeVariant)]
                        | AmbiguousTVarName LexInfo Text [(QualifiedName, Kind)]
                        deriving (Show, Eq)

data Scope = Scope {
        _scopeVars :: Map Text QualifiedName
    ,   _scopeTypes :: Map Text (QualifiedName, Kind, TypeVariant)
    ,   _scopeTVars :: Map Text (QualifiedName, Kind)
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
                alreadyInScope <- asks (member n .view (_head . scopeVars))
                if alreadyInScope 
                then throw (VarAlreadyDeclaredInScope l n)
                else local (_head . scopeVars %~ insert n n') (a n')


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
            



withDeps :: (Members '[Reader [Scope]] r) => Dependencies -> Sem r a -> Sem r a
withDeps deps a = foldr (\x r -> local (x:) r) a $ map (modSigToScope . snd) $ M.toList deps
    where
        modSigToScope (ModSig {exportedVars, exportedTypes, exportedFixities}) = Scope {
                _scopeVars      = fromList $ map (\(n,_) -> (originalName n, n)) $ M.toList exportedVars
            ,   _scopeTypes     = fromList $ map (\(n,(k,v)) -> (originalName n, (n, k, v))) $ M.toList exportedTypes
            ,   _scopeTVars     = mempty
            ,   _scopeFixities  = fromList $ map (\(n, f) -> (originalName n, f)) $ M.toList exportedFixities
            }

getConstrKind :: [TVar NextPass] -> Kind
getConstrKind = foldr (\(MkTVar _ k) r -> k `KFun` r) KStar 

qualify :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
        => Module QualifyNames
        -> Sem r (Module NextPass)
qualify (Module (Ext deps) name stmnts) = withDeps deps 
                                        $ Module (Ext deps) (unsafeQualifiedName name name (LexInfo (SourcePos 0 0) (SourcePos 0 0) name)) 
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

    (DefStruct IgnoreExt li n ps fields : sts) ->
            do
                (def, fields', n', k) <- withTVars li ps $ \ps' -> 
                    let k = getConstrKind ps' in
                --                  Ugly hack to allow recursive types :/
                    withType li n k (RecordType []) $ \qn ->
                        (\fs' -> (DefStruct (Ext k) li qn ps' fs', fs', qn, k)) 
                        <$> traverse (secondM (qualifyType li)) fields
                (def :)
                        <$> withType' li n n' k (RecordType (map (second coercePass) fields')) 
                            (qualifyStmnts sts)

    (StatementX x _ : _) -> absurd x

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
    StructConstruct IgnoreExt li structName fields -> lookupType li structName >>= \case
            (structName', _k, RecordType tyFields) -> traverse (secondM qualifyExp) fields <&> \fields' -> 
                StructConstruct (StructDef structName' (map (second coercePass) tyFields)) li structName' fields'
            (tyName', k, variant) -> throw (NotAStruct li tyName' k variant)
    StructAccess IgnoreExt li se f -> do
        allStructs <- asks (concatMap (toList . view scopeTypes))
        let possibleStructs = fromList $ allStructs & mapMaybe \(tyName, kind, tyVariant) -> case tyVariant of
                RecordType fields -> if f `elem` (map fst fields) 
                    then Just $ (tyName, StructDef tyName (map (second coercePass) fields))
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

qualifyDeclWith :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
                => QualifiedName
                -> LexInfo
                -> Decl QualifyNames
                -> Sem r (Decl NextPass)
qualifyDeclWith n' li (Decl IgnoreExt _ (Ext params) e) = 
    uncurry (Decl IgnoreExt n' . Ext)
    <$> foldr (\p r -> withVar li p \p' -> first (p' :) <$> r) (([],) <$> qualifyExp e) params

qualifyDecl :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
            => LexInfo
            -> Decl QualifyNames
            -> Sem r (Decl NextPass)
qualifyDecl li d@(Decl _ n _ _) = withVar li n \n' -> qualifyDeclWith n' li d

qualifyType :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
            => LexInfo 
            -> Type QualifyNames
            -> Sem r (Type NextPass)
qualifyType li = \case 
    TCon n ()           -> (\(n', k, _) -> TCon n' k) <$> lookupType li n
    TVar (MkTVar n ())  -> pure $ TVar (MkTVar (unsafeQualifiedName n n li) KStar)  -- TODO: Use lookupTVar
    TApp t1 t2          -> TApp <$> qualifyType li t1 <*> qualifyType li t2 
    TForall _ _         -> error "source-level foralls NYI"


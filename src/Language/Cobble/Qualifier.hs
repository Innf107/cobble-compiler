{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Qualifier where

import Language.Cobble.Prelude
import Language.Cobble.Types
import Language.Cobble.Types.Lens

import Data.Map qualified as M

type NextPass = SemAnalysis
  
type QualifyC r = Members '[State Int, State [Scope], Error QualificationError, Output Log] r

data QualificationError = NameNotFound LexInfo Text
                        | TypeNotFound LexInfo Text
                        | NotAStruct LexInfo QualifiedName
                        | VarAlreadyDeclaredInScope LexInfo Text
                        | EarlyKindMismatch (Type 'QualifyNames) (Type 'QualifyNames)
                        | LateKindMismatch (Type NextPass) (Type NextPass)
                        | InvalidKind (Type NextPass) Kind Kind
                        deriving (Show, Eq)

data Scope = Scope {
    _prefix::QualifiedName
  , _typeNames::[Text]
  , _structDefs:: Map QualifiedName (StructDef NextPass)
  , _varFunNames::[Text]
  , _typeKinds::Map Text Kind
  } deriving (Show, Eq)

makeLenses 'Scope

currentScope :: Traversal' [Scope] Scope
currentScope = _head

qualify :: (Members '[Error QualificationError, State [Scope], Output Log] r)
        => Module 'QualifyNames 
        -> Sem r (Module NextPass)
qualify = evalState @Int 0 
        . qualifyMod

newUID :: (QualifyC r) => Sem r Int
newUID = state (\s -> (s+1, s+1))

askPref :: (QualifyC r) => Sem r QualifiedName
askPref = get @[Scope] <&> view (_head . prefix)

localPref :: (QualifyC r) => (QualifiedName -> QualifiedName) -> Sem r a -> Sem r a
localPref f s = do
    modify @[Scope] (\ss -> Scope (f (ss ^. _head . prefix)) [] mempty [] mempty : ss)
    s <* modify @[Scope] (view _tail)

addName :: (QualifyC r) => LexInfo -> Text -> Sem r ()
addName li n = do
    ns <- get @[Scope] <&> view (_head . varFunNames)
    if n `elem` ns
        then throw (VarAlreadyDeclaredInScope li n)
        else modify @[Scope] (& _head . varFunNames %~ cons n)

addType :: (QualifyC r) => LexInfo -> Text -> Kind -> Sem r ()
addType li n k = do
    ns <- get @[Scope] <&> view (_head . typeNames)
    if n `elem` ns
        then throw (VarAlreadyDeclaredInScope li n)
        else do
            modify @[Scope] (& _head . typeNames %~ cons n)
            modify @[Scope] (& _head . typeKinds %~ insert n k)

qualifyMod :: (QualifyC r) => Module 'QualifyNames -> Sem r (Module NextPass)
qualifyMod (Module (Ext deps) n sts) = log LogVerbose ("QUALIFYING MODULE: " <> n) >> Module (Ext deps) (makeQName n)
    <$> runReader deps (traverse qualifyStatement sts)

qualifyStatement :: (QualifyC r, Member (Reader Dependencies) r) => Statement 'QualifyNames -> Sem r (Statement NextPass)
qualifyStatement s = log LogDebugVerbose ("QUALIFYING STATEMENT: " <> show s) >> case s of
    Def IgnoreExt li (Decl IgnoreExt n (Ext ps) e) t -> do
        n' <- askPref <&> (.: n)
        innerN <- askPref <&> (.: ("-fun_" <> n))
        addName li n
        (ps', le') <- localPref (.: ("-fun_" <> n)) $ do
            traverse_ (addName li) ps
            (,)
                <$> traverse (pure . (innerN .:)) ps
                <*> qualifyExp e
        t' <- qualifyType li t
        pure $ Def IgnoreExt li (Decl IgnoreExt n' (Ext ps') le') t'

    DefStruct IgnoreExt li n fs -> do
        n' <- askPref <&> (.: n)
        addType li n KStar
        fs' <- localPref (.: n) $ traverse (bitraverse (\x -> askPref <&> (\a -> a .: x)) (qualifyType li)) fs
        modify (& currentScope . structDefs %~ insert n' (StructDef n' fs'))
        pure $ DefStruct IgnoreExt li n' fs'
    Import IgnoreExt li modName -> pure $ Import IgnoreExt li (makeQName modName)
    StatementX x _li -> case x of

qualifyExp :: (QualifyC r, Member (Reader Dependencies) r) => Expr 'QualifyNames -> Sem r (Expr NextPass)
qualifyExp e = do
    log LogDebugVerbose ("QUALIFYING EXPR: " <> show e)
    res <- go
    log LogDebugVeryVerbose ("RESULT: " <> show res)
    pure res
    where
        go = case e of
            FCall IgnoreExt li f ps -> do
                f' <- qualifyExp f
                ps' <- traverse qualifyExp ps
                pure $ FCall IgnoreExt li f' ps'
            IntLit IgnoreExt li i -> pure $ IntLit IgnoreExt li i
            UnitLit li -> pure $ UnitLit li
            If  IgnoreExt li c th el -> do
                ifeID <- newUID
                let thName = "-then-e" <> show ifeID
                let elName = "-else-e" <> show ifeID
                c' <- qualifyExp c
                th' <- localPref (.: thName) $ qualifyExp th
                el' <- localPref (.: elName) $ qualifyExp el
                name <- askPref
                pure (If (Ext (name, ifeID)) li c' th' el')
            Let IgnoreExt li (Decl IgnoreExt vname (Ext ps) expr) body -> do
                vname' <- askPref <&> (.: vname)
                innerN <- askPref <&> (.: ("-let-" <> vname <> "-body"))
                addName li vname
                expr' <- localPref (.: ("-let-" <> show vname')) $ qualifyExp expr
                (ps', body') <- localPref (.: ("-let-" <> vname <> "-body")) $ do
                    traverse_ (addName li) ps
                    (,)
                        <$> traverse (pure . (innerN .:)) ps
                        <*> qualifyExp body
                pure (Let IgnoreExt li (Decl IgnoreExt vname' (Ext ps') expr') body')
            Var IgnoreExt li vname -> Var IgnoreExt li <$> lookupName vname li
            StructConstruct IgnoreExt li cname fs -> do
                cname' <- lookupTypeName cname li
                structDef <- lookupStructDef cname' li
                fs' <- traverse (bitraverse (pure . (cname' .:)) qualifyExp) fs
                pure (StructConstruct structDef li cname' fs')
            StructAccess IgnoreExt li structEx fname -> do
                structs <- (<>) <$> gets @[Scope] (fromList . map (\sd -> (view structName sd, sd)) . toListOf (folded . structDefs . folded))
                                <*> asks (M.foldMapWithKey (\_ -> M.mapMaybeWithKey (\n (_, x) -> case x of
                                    RecordType fs -> Just (coercePass $ StructDef n fs)
                                    _ -> Nothing
                                ) . exportedTypes))
                StructAccess structs li
                    <$> qualifyExp structEx
                    <*> pure fname
            ExprX x _li -> absurd x


qualifyType :: forall r. (QualifyC r, Member (Reader Dependencies) r) => LexInfo -> Type 'QualifyNames -> Sem r (Type NextPass)
qualifyType li t = log LogDebugVeryVerbose ("QUALIFYING TYPE: " <> show t) >> qtInner t
    where 
        qtInner :: Type 'QualifyNames -> Sem r (Type NextPass)
        qtInner = \case
            TCon n ()   -> do
                TCon <$> lookupTypeName n li <*> lookupKind n li
            TApp t1 t2  -> TApp <$> qtInner t1 <*> qtInner t2
            TVar (MkTVar v ())   -> pure $ TVar $ MkTVar (QualifiedName [v]) KStar -- TODO

lookupName :: (QualifyC r, Member (Reader (Map QualifiedName ModSig)) r) => Text -> LexInfo -> Sem r QualifiedName
lookupName n li = get @[Scope] >>= \scopes -> ask >>= \deps -> do
    log LogDebugVeryVerbose ("LOOKING UP NAME '" <> show n <> "' IN " <> show scopes)
    maybe (throw (NameNotFound li n)) pure $
        (flip asumMap scopes $ \s -> whenAlt (n `elem` _varFunNames s) (_prefix s .: n))
        <|> flip asumMap (M.toList deps) (\(modName, ModSig{exportedVars}) 
            -> if (modName .: n) `member` (M.keysSet exportedVars) then Just (modName .: n) else Nothing)
lookupTypeName :: (QualifyC r, Member (Reader (Map QualifiedName ModSig)) r) => Text -> LexInfo -> Sem r QualifiedName
lookupTypeName n li = get @[Scope] >>= \scopes -> ask >>= \deps -> do
    log LogDebugVeryVerbose ("LOOKING UP Type '" <> show n <> "' IN " <> show scopes <> " AND " <> show deps)
    maybe (throw (TypeNotFound li n)) pure $
        flip asumMap scopes (\s -> whenAlt (n `elem` _typeNames s) (_prefix s .: n))
        <|> flip asumMap (M.toList deps) (\(modName, ModSig{exportedTypes}) -> lookup (modName .: n) exportedTypes $> modName .: n)
lookupStructDef :: (Members [Reader Dependencies, State [Scope], Error QualificationError, Output Log] r)
                => QualifiedName
                -> LexInfo
                -> Sem r (StructDef NextPass)
lookupStructDef n li = get @[Scope] >>= \scopes -> ask >>= \deps -> do
    log LogDebugVeryVerbose ("LOOKING UP StructDef '" <> show n <> "' IN " <> show scopes <> " AND " <> show deps)
    maybe (throw (NotAStruct li n)) pure $
        (scopes & asumMap (lookup n . view structDefs))
        <|> (M.toList deps & asumMap (\(_, s) -> do
            (_, RecordType fs) <- lookup n (exportedTypes s)
            Just $ coercePass (StructDef n fs)))

lookupKind :: (QualifyC r, Member (Reader (Map QualifiedName ModSig)) r) => Text -> LexInfo -> Sem r Kind
lookupKind n li = get @[Scope] >>= \scopes -> ask >>= \deps -> do
    log LogDebugVeryVerbose ("LOOKING UP KIND OF NAME '" <> show n <> "' IN " <> show scopes <> " AND " <> show deps)
    maybe (throw (TypeNotFound li n)) pure $
        flip asumMap scopes $ \s -> lookup n (_typeKinds s)
        <|> flip asumMap (M.toList deps) (\(modName, ModSig{exportedTypes}) -> fst <$> (lookup (modName .: n) exportedTypes))


kind' :: (QualifyC r) => Type NextPass -> Sem r Kind
kind' t = either (throw . uncurry (InvalidKind t)) pure (kind t)

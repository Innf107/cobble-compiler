{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Qualifier where

import Language.Cobble.Prelude
import Language.Cobble.Types

import Data.Map qualified as M

type NextPass = 'Typecheck
  
type QualifyC r = Members '[State Int, State [Scope], Error QualificationError, Output Log] r

data QualificationError = NameNotFound LexInfo Text
                        | TypeNotFound LexInfo Text
                        | VarAlreadyDeclaredInScope LexInfo Text
                        | EarlyKindMismatch (Type 'QualifyNames) (Type 'QualifyNames)
                        | LateKindMismatch (Type NextPass) (Type NextPass)
                        | InvalidKind (Type NextPass) Kind Kind
                        deriving (Show, Eq)

data Scope = Scope {
    _prefix::QualifiedName
  , _typeNames::[Text]
  , _varFunNames::[Text]
  , _typeKinds::Map Text Kind
  } deriving (Show, Eq)

makeLenses 'Scope

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
    modify @[Scope] (\ss -> Scope (f (ss ^. _head . prefix)) [] [] mempty : ss)
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
qualifyMod (Module deps n sts) = log LogVerbose ("QUALIFYING MODULE: " <> n) >> Module deps (makeQName n)
    <$> runReader deps (traverse qualifyStatement sts)

qualifyStatement :: (QualifyC r, Member (Reader Dependencies) r) => Statement 'QualifyNames -> Sem r (Statement NextPass)
qualifyStatement s = log LogDebugVerbose ("QUALIFYING STATEMENT: " <> show s) >> case s of
    Def () li n ps e t -> do
        n' <- askPref <&> (.: n)
        innerN <- askPref <&> (.: ("-fun_" <> n))
        addName li n
        (ps', le') <- localPref (.: ("-fun_" <> n)) $ do
            traverse_ (addName li) ps
            (,)
                <$> traverse (pure . (innerN .:)) ps
                <*> qualifyExp e
        t' <- qualifyType li t
        pure $ Def () li n' ps' le' t'

    DefStruct () li n fs -> do
        n' <- askPref <&> (.: n)
        addType li n KStar
        fs' <- localPref (.: n) $ traverse (bitraverse (\x -> askPref <&> (\a -> a .: x)) (qualifyType li)) fs
        pure $ DefStruct () li n' fs'
    Import () li modName -> pure $ Import () li (makeQName modName)
    StatementX x _li -> case x of

qualifyLogSeg :: (QualifyC r,  Member (Reader Dependencies) r) => LexInfo -> LogSegment 'QualifyNames -> Sem r (LogSegment NextPass)
qualifyLogSeg _li (LogText t) = pure $ LogText t
qualifyLogSeg li (LogVar v) = LogVar <$> lookupName v li

qualifyExp :: (QualifyC r, Member (Reader Dependencies) r) => Expr 'QualifyNames -> Sem r (Expr NextPass)
qualifyExp e = do
    log LogDebugVerbose ("QUALIFYING EXPR: " <> show e)
    res <- go
    log LogDebugVeryVerbose ("RESULT: " <> show res)
    pure res
    where
        go = case e of
            FCall () li f ps -> do
                f' <- qualifyExp f
                ps' <- traverse qualifyExp ps
                pure $ FCall () li f' ps'
            IntLit () li i -> pure $ IntLit () li i
            UnitLit li -> pure $ UnitLit li
            If  () li c th el -> do
                ifeID <- newUID
                let thName = "-then-e" <> show ifeID
                let elName = "-else-e" <> show ifeID
                c' <- qualifyExp c
                th' <- localPref (.: thName) $ qualifyExp th
                el' <- localPref (.: elName) $ qualifyExp el
                name <- askPref
                pure (If (name, ifeID) li c' th' el')
            
            Var () li vname -> Var () li <$> lookupName vname li
            ExprX x _li -> case x of


qualifyType :: forall r. (QualifyC r, Member (Reader Dependencies) r) => LexInfo -> Type 'QualifyNames -> Sem r (Type NextPass)
qualifyType li t = log LogDebugVeryVerbose ("QUALIFYING TYPE: " <> show t) >> qtInner t
    where 
        qtInner :: Type 'QualifyNames -> Sem r (Type NextPass)
        qtInner = \case
            TCon n ()   -> do
                TCon <$> lookupTypeName n li <*> lookupKind n li
            TApp t1 t2  -> TApp <$> qtInner t1 <*> qtInner t2
            TVar v ()   -> pure $ TVar (QualifiedName [v]) KStar -- TODO

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

lookupKind :: (QualifyC r, Member (Reader (Map QualifiedName ModSig)) r) => Text -> LexInfo -> Sem r Kind
lookupKind n li = get @[Scope] >>= \scopes -> ask >>= \deps -> do
    log LogDebugVeryVerbose ("LOOKING UP KIND OF NAME '" <> show n <> "' IN " <> show scopes <> " AND " <> show deps)
    maybe (throw (TypeNotFound li n)) pure $
        flip asumMap scopes $ \s -> lookup n (_typeKinds s)
        <|> flip asumMap (M.toList deps) (\(modName, ModSig{exportedTypes}) -> fst <$> (lookup (modName .: n) exportedTypes))


kind' :: (QualifyC r) => Type NextPass -> Sem r Kind
kind' t = either (throw . uncurry (InvalidKind t)) pure (kind t)

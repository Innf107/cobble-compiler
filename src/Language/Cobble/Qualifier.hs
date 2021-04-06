{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Qualifier where

import Language.Cobble.Prelude
import Language.Cobble.Types

import Data.Map qualified as M

type NextPass = 'Typecheck
  
type QualifyC r = Members '[State Int, State [Scope], Error QualificationError] r

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
  }

makeLenses 'Scope

qualify :: (Members '[Error QualificationError, State [Scope]] r) 
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
qualifyMod (Module deps n sts) = Module deps (makeQName n) <$> traverse qualifyStatement sts

qualifyStatement :: (QualifyC r) => Statement 'QualifyNames -> Sem r (Statement NextPass)
qualifyStatement = \case
    CallFun () li n args -> CallFun () li
                        <$> lookupName n li
                        <*> traverse qualifyExp args
    DefVoid () li n ps body -> do
        n' <- askPref <&> (.: n)
        innerN <- askPref <&> (.: ("@fun_" <> n))
        addName li n
        (ps', body') <- localPref (.: ("@fun_" <> n)) $ do
            traverse_ (addName li . fst) ps
            (,)
                <$> traverse (bitraverse (pure . (innerN .:)) (qualifyType li)) ps
                <*> traverse qualifyStatement body
        pure $ DefVoid () li n' ps' body'
        
    DefFun () li n ps body le t -> do
        n' <- askPref <&> (.: n)
        innerN <- askPref <&> (.: ("@fun_" <> n))
        addName li n
        (ps', body', le') <- localPref (.: ("@fun_" <> n)) $ do
            traverse_ (addName li . fst) ps
            (,,)
                <$> traverse (bitraverse (pure . (innerN .:)) (qualifyType li)) ps
                <*> traverse qualifyStatement body
                <*> qualifyExp le
        t' <- qualifyType li t
        pure $ DefFun () li n' ps' body' le' t'

    Decl () li n mt e -> do
        n' <- askPref <&> (.: n)
        addName li n
        mt' <- traverse (qualifyType li) mt
        e' <- qualifyExp e
        pure $ Decl () li n' mt' e'
        
    Assign () li n e -> do
        n' <- lookupName n li
        e' <- qualifyExp e
        pure $ Assign () li n' e'

    While () li c body -> do
        whileName <- (("@while" <>) . show) <$> newUID
        c' <- qualifyExp c
        body' <- localPref (.: whileName) $ traverse qualifyStatement body
        pure $ While () li c' body'
    DefStruct () li n fs -> do
        n' <- askPref <&> (.: n)
        addType li n KStar
        fs' <- localPref (.: n) $ traverse (bitraverse (\x -> askPref <&> (\a -> a .: x)) (qualifyType li)) fs
        pure $ DefStruct () li n' fs'

qualifyExp :: (QualifyC r) => Expr 'QualifyNames -> Sem r (Expr NextPass)
qualifyExp = \case
    FCall () li fname ps -> do
        fname' <- lookupName fname li
        ps' <- traverse qualifyExp ps
        pure $ FCall () li fname' ps'
    IntLit () li i -> pure $ IntLit () li i
    BoolLit () li b -> pure $ BoolLit () li b
    Var () li vname -> Var () li <$> lookupName vname li

-- TODO: Fix Kind inference
qualifyType :: forall r. (QualifyC r) => LexInfo -> Type 'QualifyNames -> Sem r (Type NextPass)
qualifyType li = qtInner KStar
    where 
        qtInner :: Kind -> Type 'QualifyNames -> Sem r (Type NextPass)
        qtInner k = \case
            TCon n ()   -> do
                TCon <$> lookupTypeName n li <*> lookupKind n li
            TApp t1 t2  -> do
                t1' <- qtInner (KStar `KFun` k) t1
                kind' t1' >>= \case
                    KFun k1 k2 -> do
                        t2' <- qtInner k1 t2
                        pure $ TApp t1' t2'
                    _ -> throw $ EarlyKindMismatch t1 t2
            TVar v ()   -> pure $ TVar (QualifiedName [v]) k

lookupName :: (QualifyC r) => Text -> LexInfo -> Sem r QualifiedName
lookupName n li = get @[Scope] >>= \scopes -> maybe (throw (NameNotFound li n)) pure $
    flip asumMap scopes $ \s -> whenAlt (n `elem` _varFunNames s) (_prefix s .: n)

lookupTypeName :: (QualifyC r) => Text -> LexInfo -> Sem r QualifiedName
lookupTypeName n li = get @[Scope] >>= \scopes -> maybe (throw (TypeNotFound li n)) pure $
    flip asumMap scopes $ \s -> whenAlt (n `elem` _typeNames s) (_prefix s .: n)

lookupKind :: (QualifyC r) => Text -> LexInfo -> Sem r Kind
lookupKind n li = get @[Scope] >>= \scopes -> maybe (throw (TypeNotFound li n)) pure $
    flip asumMap scopes $ \s -> lookup n (_typeKinds s)


kind' :: (QualifyC r) => Type NextPass -> Sem r Kind
kind' t = either (throw . uncurry (InvalidKind t)) pure (kind t)

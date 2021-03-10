{-# LANGUAGE NoImplicitPrelude, DataKinds, ConstraintKinds, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications, TemplateHaskell #-}
module Language.Cobble.Qualifier where

import Language.Cobble.Prelude
import Language.Cobble.Types
  
type NextPass = 'Typecheck
  
type QualifyC r = Members '[State Int, State [Scope], Error QualificationError] r

data QualificationError = NameNotFound LexInfo Text
                        | TypeNotFound LexInfo Text
                        | VarAlreadyDeclaredInScope LexInfo Text
                        deriving (Show, Eq)

data Scope = Scope {
    _prefix::QualifiedName
  , _typeNames::[Text]
  , _varFunNames::[Text]
  }

makeLenses 'Scope

newUID :: (QualifyC r) => Sem r Int
newUID = state (\s -> (s+1, s+1))

askPref :: (QualifyC r) => Sem r QualifiedName
askPref = get @[Scope] <&> view (_head . prefix)

localPref :: (QualifyC r) => (QualifiedName -> QualifiedName) -> Sem r a -> Sem r a
localPref f s = do
    modify @[Scope] (\ss -> Scope (f (ss ^. _head . prefix)) [] [] : ss)
    s <* modify @[Scope] (view _tail)

addName :: (QualifyC r) => LexInfo -> Text -> Sem r ()
addName li n = do
    ns <- get @[Scope] <&> view (_head . varFunNames)
    if n `elem` ns
        then throw (VarAlreadyDeclaredInScope li n)
        else modify @[Scope] (& _head . varFunNames %~ cons n)

addType :: (QualifyC r) => LexInfo -> Text -> Sem r ()
addType li n = do
    ns <- get @[Scope] <&> view (_head . typeNames)
    if n `elem` ns
        then throw (VarAlreadyDeclaredInScope li n)
        else modify @[Scope] (& _head . typeNames %~ cons n)


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
        addType li n
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

qualifyType :: (QualifyC r) => LexInfo -> Type 'QualifyNames -> Sem r (Type NextPass)
qualifyType li = \case
    IntT -> pure IntT
    BoolT -> pure BoolT
    EntityT -> pure EntityT
    StructT n -> StructT <$> lookupTypeName n li

lookupName :: (QualifyC r) => Text -> LexInfo -> Sem r QualifiedName
lookupName n li = get @[Scope] >>= \scopes -> maybe (throw (NameNotFound li n)) pure $
    flip asumMap scopes $ \s -> whenAlt (n `elem` _varFunNames s) (_prefix s .: n)

lookupTypeName :: (QualifyC r) => Text -> LexInfo -> Sem r QualifiedName
lookupTypeName n li = get @[Scope] >>= \scopes -> maybe (throw (TypeNotFound li n)) pure $
    flip asumMap scopes $ \s -> whenAlt (n `elem` _typeNames s) (_prefix s .: n)


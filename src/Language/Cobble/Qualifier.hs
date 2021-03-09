{-# LANGUAGE NoImplicitPrelude, DataKinds, ConstraintKinds, LambdaCase #-}
module Language.Cobble.Qualifier where

import Language.Cobble.Prelude
import Language.Cobble.Types
  
type NextPass = 'Typecheck
  
type QualifyC r = Members '[Reader QualifiedName] r

qualifyStatement :: (QualifyC r) => Statement 'QualifyNames -> Sem r (Statement NextPass)
qualifyStatement = \case
    CallFun () li n args -> CallFun () li
                        <$> lookupFunName n
                        <*> traverse qualifyExp args
    DefVoid () li n ps body -> do
        n' <- ask <&> (.: n)
        ps' <- traverse (bitraverse (pure . (n' .:)) qualifyType) ps
        body' <- local (.: n) $ traverse qualifyStatement body
        pure $ DefVoid () li n' ps' body'
        
    DefFun () li n ps body le t -> do
        n' <- ask <&> (.: n)
        ps' <- traverse (bitraverse (pure . (n' .:)) qualifyType) ps
        body' <- local (.: n) $ traverse qualifyStatement body
        le' <- local (.: n) $ qualifyExp le
        t' <- qualifyType t
        pure $ DefFun () li n' ps' body' le' t'
        
    Decl () li n mt e -> do
        n' <- ask <&> (.: n)
        mt' <- traverse qualifyType mt
        e' <- local (.: n) $ qualifyExp e
        pure $ Decl () li n' mt' e'
        
    Assign () li n e -> do
        n' <- lookupVarName n
        e' <- qualifyExp e
        pure $ Assign () li n' e'
        
    --TODO
        
qualifyExp :: (QualifyC r) => Expr 'QualifyNames -> Sem r (Expr NextPass) 
qualifyExp = undefined

lookupFunName :: (QualifyC r) => Text -> Sem r QualifiedName
lookupFunName = undefined

lookupVarName :: (QualifyC r) => Text -> Sem r QualifiedName
lookupVarName = undefined

qualifyType :: (QualifyC r) => Type 'QualifyNames -> Sem r (Type NextPass)
qualifyType = undefined

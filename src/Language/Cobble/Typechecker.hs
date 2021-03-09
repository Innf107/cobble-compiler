{-#LANGUAGE NoImplicitPrelude, ConstraintKinds, DataKinds, LambdaCase, TupleSections#-}
{-# LANGUAGE ViewPatterns #-}
module Language.Cobble.Typechecker where

import Language.Cobble.Prelude
import Language.Cobble.Util.Convert
import Language.Cobble.Types

type NextPass = 'Codegen

data TypeError = VarDoesNotExist LexInfo (Name NextPass)
               | FunctionDoesNotExist LexInfo (Name NextPass)
--               ^ also thrown if void is expected to return (e.g. when called as an expression)
               | WrongFunArgs LexInfo (Name NextPass) [Type NextPass] [Type NextPass]
--                                       ^ expected
               | WrongReturnType LexInfo (Name NextPass) (Type NextPass) (Type NextPass)
--                                         ^ expected
               | WrongDeclType LexInfo (Name NextPass) (Type NextPass) (Type NextPass)
--                                  ^ expected
               | WrongAssignType LexInfo (Name NextPass) (Type NextPass) (Type NextPass)
--                                    ^ expected
               deriving (Show, Eq)

-- TODO: Scope (Maybe should be handled in a different step?)
data TCState = TCState {
        varTypes::Map (Name 'Typecheck) (Type NextPass)
      , funReturnTypes::Map (Name 'Typecheck) (Type NextPass)
      , funArgs::Map (Name 'Typecheck) [Type NextPass]
    } deriving (Show, Eq)


type TypecheckC r = Members [State TCState, Error TypeError] r

initialTCState :: TCState
initialTCState = TCState {
        varTypes = mempty
      , funReturnTypes = mempty
      , funArgs = mempty
    }

getVarType :: (TypecheckC r) => LexInfo -> Name 'Typecheck -> Sem r (Type NextPass)
getVarType l varName = gets varTypes <&> lookup varName >>= \case
    Nothing -> throw (VarDoesNotExist l varName)
    Just t -> pure t

getFunReturnType :: (TypecheckC r) => LexInfo -> Name 'Typecheck -> Sem r (Type NextPass)
getFunReturnType l funName = gets funReturnTypes <&> lookup funName >>= \case
    Nothing -> throw (FunctionDoesNotExist l funName)
    Just t -> pure t
    
getFunArgs :: (TypecheckC r) => LexInfo -> Name 'Typecheck -> Sem r [Type NextPass]
getFunArgs l funName = gets funArgs <&> lookup funName >>= \case
    Nothing -> throw (FunctionDoesNotExist l funName)
    Just as -> pure as

insertFunArgs :: (TypecheckC r) => Name NextPass -> [Type NextPass] -> Sem r ()
insertFunArgs funName ts =void $ modify (\s -> s{funArgs=funArgs s & insert funName ts})

insertFunReturnType :: (TypecheckC r) => Name NextPass -> Type NextPass -> Sem r ()
insertFunReturnType funName t = void $ modify (\s -> s{varTypes=funReturnTypes s & insert funName t})

insertVarType :: (TypecheckC r) => Name NextPass -> Type NextPass -> Sem r ()
insertVarType varName t = void $ modify (\s -> s{varTypes=varTypes s & insert varName t})

typecheckModule :: (TypecheckC r) => Module 'Typecheck -> Sem r (Module NextPass)
typecheckModule (Module mname instrs) = Module mname <$> traverse typecheck instrs

typecheck :: (TypecheckC r) => Statement 'Typecheck -> Sem r (Statement NextPass)
typecheck = \case
    --CallVoid fname exprs -> do
    --    fargs <- getFunArgs fname
    --    exprs' <- traverse typeOf exprs
    --    let exprTypes = map snd exprs'
    --    if (exprTypes /= fargs)
    --        then throw $ MisMatchedFunArgs fname fargs exprTypes
    --        else pure (CallVoid fname exprs')
    -- TODO: Handle void functions
    CallFunU l fname exprs -> do
        fargs <- getFunArgs l fname
        exprs' <- traverse typeOf exprs
        
        let exprTypes = map exprType exprs'
        
        if (exprTypes /= fargs)
            then throw $ WrongFunArgs l fname fargs exprTypes
            else pure (CallFunT l fname exprs')
    DefVoidU l fname (conv -> args) stmnts -> do
        insertFunArgs fname (map snd args)
        for_ args (uncurry insertVarType)
        stmnts' <- traverse typecheck stmnts
        pure (DefVoidT l fname args stmnts')
    DefFunU l fname (conv -> args) stmnts lastexpr (conv -> t) -> do
        insertFunArgs fname (map snd args)
        for_ args (uncurry insertVarType)
        stmnts' <- traverse typecheck stmnts
        lastexpr' <- typeOf lastexpr
        if (exprType lastexpr' == t)
        then insertFunReturnType fname t >> pure (DefFunT l fname args stmnts' lastexpr' t)
        else throw (WrongReturnType l fname t (exprType lastexpr'))
    DeclU l vname (Just (conv -> t)) expr -> do
        expr' <- typeOf expr
        if (exprType expr' == t)
        then insertVarType vname t >> pure (DeclT l vname (Just t) expr')
        else throw (WrongDeclType l vname t (exprType expr'))
    DeclU l vname Nothing expr -> do
        expr' <- typeOf expr
        insertVarType vname (exprType expr') >> pure (DeclT l vname Nothing expr')
    AssignU l vname expr -> do
        varT <- getVarType l vname
        expr' <- typeOf expr
        if (varT == exprType expr')
        then pure (AssignT l vname expr')
        else throw (WrongAssignType l vname varT (exprType expr'))
    WhileU l cond stmnts -> do
        cond' <- typeOf cond
        stmnts' <- traverse typecheck stmnts
        pure (WhileT l cond' stmnts')
    DefStructU l name (conv -> fields) -> pure $ DefStructT l name fields -- TODO: Add to state map

typeOf :: (TypecheckC r) => Expr 'Typecheck -> Sem r (Expr NextPass)
typeOf = \case
    IntLitU l x -> pure $ IntLitT l x
    -- FloatLit x -> pure (FloatLit x, FloatT)
    BoolLitU l x -> pure $ BoolLitT l x
    FCallU l fname exprs -> do
        fargs <- getFunArgs l fname
        exprs' <- traverse typeOf exprs
        let exprTypes = map exprType exprs' 
        if (exprTypes == fargs)
        then (\x -> FCallT x l fname exprs') <$> getFunReturnType l fname
        else throw $ WrongFunArgs l fname fargs exprTypes
    VarU l vname -> (\x -> VarT x l vname) <$> getVarType l vname



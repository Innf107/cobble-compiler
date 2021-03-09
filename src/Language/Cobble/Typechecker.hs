{-#LANGUAGE NoImplicitPrelude, ConstraintKinds, DataKinds, LambdaCase, TupleSections#-}
module Language.Cobble.Typechecker where

import Language.Cobble.Prelude
import Language.Cobble.Types


data TypeError = VarDoesNotExist LexInfo Name
               | FunctionDoesNotExist LexInfo Name
--               ^ also thrown if void is expected to return (e.g. when called as an expression)
               | WrongFunArgs LexInfo Name [Type] [Type]
--                                       ^ expected
               | WrongReturnType LexInfo Name Type Type
--                                         ^ expected
               | WrongDeclType LexInfo Name Type Type
--                                  ^ expected
               | WrongAssignType LexInfo Name Type Type
--                                    ^ expected
               deriving (Show, Eq)

-- TODO: Scope (Maybe should be handled in a different step?)
data TCState = TCState {
        varTypes::Map Name Type
      , funReturnTypes::Map Name Type
      , funArgs::Map Name [Type]
    } deriving (Show, Eq)


type TypecheckC r = Members [State TCState, Error TypeError] r

initialTCState :: TCState
initialTCState = TCState {
        varTypes = mempty
      , funReturnTypes = mempty
      , funArgs = mempty
    }

getVarType :: (TypecheckC r) => LexInfo -> Name -> Sem r Type
getVarType l varName = gets varTypes <&> lookup varName >>= \case
    Nothing -> throw (VarDoesNotExist l varName)
    Just t -> pure t

getFunReturnType :: (TypecheckC r) => LexInfo -> Name -> Sem r Type
getFunReturnType l funName = gets funReturnTypes <&> lookup funName >>= \case
    Nothing -> throw (FunctionDoesNotExist l funName)
    Just t -> pure t

getFunArgs :: (TypecheckC r) => LexInfo -> Name -> Sem r [Type]
getFunArgs l funName = gets funArgs <&> lookup funName >>= \case
    Nothing -> throw (FunctionDoesNotExist l funName)
    Just as -> pure as

insertFunArgs :: (TypecheckC r) => Name -> [Type] -> Sem r ()
insertFunArgs funName ts =void $ modify (\s -> s{funArgs=funArgs s & insert funName ts})

insertFunReturnType :: (TypecheckC r) => Name -> Type -> Sem r ()
insertFunReturnType funName t = void $ modify (\s -> s{varTypes=funReturnTypes s & insert funName t})

insertVarType :: (TypecheckC r) => Name -> Type -> Sem r ()
insertVarType varName t = void $ modify (\s -> s{varTypes=varTypes s & insert varName t})

typecheckModule :: (TypecheckC r) => Module 'Unaltered -> Sem r (Module 'Typed)
typecheckModule (Module mname instrs) = Module mname <$> traverse typecheck instrs

typecheck :: (TypecheckC r) => Statement 'Unaltered -> Sem r (Statement 'Typed)
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
    DefVoidU l fname args stmnts -> do
        insertFunArgs fname (map snd args)
        for_ args (uncurry insertVarType)
        stmnts' <- traverse typecheck stmnts
        pure (DefVoidT l fname args stmnts')
    DefFunU l fname args stmnts lastexpr t -> do
        insertFunArgs fname (map snd args)
        for_ args (uncurry insertVarType)
        stmnts' <- traverse typecheck stmnts
        lastexpr' <- typeOf lastexpr
        if (exprType lastexpr' == t)
        then insertFunReturnType fname t >> pure (DefFunT l fname args stmnts' lastexpr' t)
        else throw (WrongReturnType l fname t (exprType lastexpr'))
    DeclU l vname (Just t) expr -> do
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
    DefStructU l name fields -> pure $ DefStructT l name fields -- TODO: Add to state map

typeOf :: (TypecheckC r) => Expr 'Unaltered -> Sem r (Expr 'Typed)
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



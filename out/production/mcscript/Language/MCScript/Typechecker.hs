{-#LANGUAGE NoImplicitPrelude, ConstraintKinds, DataKinds, LambdaCase, TupleSections#-}
module Language.MCScript.Typechecker where

import Language.MCScript.Prelude
import Language.MCScript.Types


data TypeError = VarDoesNotExist Name
               | FunctionDoesNotExist Name
--               ^ also thrown if void is expected to return (e.g. when called as an expression)
               | WrongFunArgs Name [Type] [Type]
--                                       ^ expected
               | WrongReturnType Name Type Type
--                                         ^ expected
               | WrongDeclType Name Type Type
--                                  ^ expected
               | WrongAssignType Name Type Type
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

getVarType :: (TypecheckC r) => Name -> Sem r Type
getVarType varName = gets varTypes <&> lookup varName >>= \case
    Nothing -> throw (VarDoesNotExist varName)
    Just t -> pure t

getFunReturnType :: (TypecheckC r) => Name -> Sem r Type
getFunReturnType funName = gets funReturnTypes <&> lookup funName >>= \case
    Nothing -> throw (FunctionDoesNotExist funName)
    Just t -> pure t

getFunArgs :: (TypecheckC r) => Name -> Sem r [Type]
getFunArgs funName = gets funArgs <&> lookup funName >>= \case
    Nothing -> throw (FunctionDoesNotExist funName)
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
    CallFunU fname exprs -> do
        fargs <- getFunArgs fname
        exprs' <- traverse typeOf exprs
        
        let exprTypes = map exprType exprs'
        
        if (exprTypes /= fargs)
            then throw $ WrongFunArgs fname fargs exprTypes
            else pure (CallFun void_ fname exprs')
    DefVoidU fname args stmnts -> do
        insertFunArgs fname (map snd args)
        for_ args (uncurry insertVarType)
        stmnts' <- traverse typecheck stmnts
        pure (DefVoid void_ fname args stmnts')
    DefFunU fname args stmnts lastexpr t -> do
        insertFunArgs fname (map snd args)
        for_ args (uncurry insertVarType)
        stmnts' <- traverse typecheck stmnts
        lastexpr' <- typeOf lastexpr
        if (exprType lastexpr' == t)
        then insertFunReturnType fname t >> pure (DefFun void_ fname args stmnts' lastexpr' t)
        else throw (WrongReturnType fname t (exprType lastexpr'))
    DeclU vname (Just t) expr -> do
        expr' <- typeOf expr
        if (exprType expr' == t)
        then insertVarType vname t >> pure (Decl void_ vname (Just t) expr')
        else throw (WrongDeclType vname t (exprType expr'))
    DeclU vname Nothing expr -> do
        expr' <- typeOf expr
        insertVarType vname (exprType expr') >> pure (Decl void_ vname Nothing expr')        
    AssignU vname expr -> do
        varT <- getVarType vname
        expr' <- typeOf expr
        if (varT == exprType expr')
        then pure (Assign void_ vname expr') 
        else throw (WrongAssignType vname varT (exprType expr'))
    WhileU cond stmnts -> do
        cond' <- typeOf cond
        stmnts' <- traverse typecheck stmnts
        pure (While void_ cond' stmnts')
    DefStructU name fields -> pure $ DefStructT name fields -- TODO: Add to state map

typeOf :: (TypecheckC r) => Expr 'Unaltered -> Sem r (Expr 'Typed)
typeOf = \case
    IntLit _ x -> pure $ IntLit void_ x
    -- FloatLit x -> pure (FloatLit x, FloatT)
    BoolLit _ x -> pure $ BoolLit void_ x
    FCall _ fname exprs -> do
        fargs <- getFunArgs fname
        exprs' <- traverse typeOf exprs
        let exprTypes = map exprType exprs' 
        if (exprTypes == fargs)
        then (\x -> FCall x fname exprs') <$> getFunReturnType fname
        else throw $ WrongFunArgs fname fargs exprTypes
    Var _ vname -> (\x -> Var x vname) <$> getVarType vname



{-#LANGUAGE NoImplicitPrelude, ConstraintKinds, DataKinds, LambdaCase, TupleSections#-}
module Language.MCScript.Typechecker where

import Language.MCScript.Prelude
import Language.MCScript.Types


data TypeError = VarDoesNotExist Name
               | FunctionDoesNotExist Name
--               ^ also thrown if void is expected to return (e.g. when called as an expression)
               | MisMatchedFunArgs Name [Type] [Type]
--                                       ^ expected
               | MisMatchedReturnType Name Type Type
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

typecheckModule :: (TypecheckC r) => Module 'Untyped -> Sem r (Module 'Typed)
typecheckModule (Module mname instrs) = Module mname <$> traverse typecheck instrs

typecheck :: (TypecheckC r) => Statement 'Untyped -> Sem r (Statement 'Typed)
typecheck = \case
    CallVoid fname exprs -> do
       fargs <- getFunArgs fname
       exprs' <- traverse typeOf exprs
       let exprTypes = map snd exprs'
       when (exprTypes /= fargs) $ throw $ MisMatchedFunArgs fname fargs exprTypes
       pure (CallVoid fname exprs')
    CallFun fname exprs -> do
        fc' <- typeOf (FCall fname exprs)
        let (FCall _ exprs', _) = fc'
        pure (CallFun fname exprs')
    DefVoid fname args stmnts -> do
        insertFunArgs fname (map snd args)
        for_ args (uncurry insertVarType)
        stmnts' <- traverse typecheck stmnts
        pure (DefVoid fname args stmnts')
    DefFun fname args stmnts lastexpr t -> do
        insertFunArgs fname (map snd args)
        for_ args (uncurry insertVarType)
        stmnts' <- traverse typecheck stmnts
        lastexpr' <- typeOf lastexpr
        if (snd lastexpr' == t)
        then insertFunReturnType fname t >> pure (DefFun fname args stmnts' lastexpr' t)
        else throw (MisMatchedReturnType fname t (snd lastexpr'))
    Decl vname t expr -> do
        expr' <- typeOf expr
        if (snd expr' == t)
        then insertVarType vname t >> pure (Decl vname t expr')
        else throw (WrongDeclType vname t (snd expr'))
    Assign vname expr -> do
        varT <- getVarType vname
        expr' <- typeOf expr
        if (varT == snd expr')
        then pure (Assign vname expr') 
        else throw (WrongAssignType vname varT (snd expr'))
    While cond stmnts -> do
        cond' <- typeOf cond
        stmnts' <- traverse typecheck stmnts
        pure (While cond' stmnts')
    DefStruct name fields -> pure $ DefStruct name fields -- TODO: Add to state map

typeOf :: (TypecheckC r) => Expr 'Untyped -> Sem r (Expr 'Typed, Type)
typeOf = \case
    IntLit x -> pure (IntLit x, IntT)
    -- FloatLit x -> pure (FloatLit x, FloatT)
    BoolLit x -> pure (BoolLit x, BoolT)
    FCall fname exprs -> do
        fargs <- getFunArgs fname
        exprs' <- traverse typeOf exprs
        let exprTypes = map snd exprs' 
        if (exprTypes == fargs)
        then (FCall fname exprs',) <$> getFunReturnType fname
        else throw $ MisMatchedFunArgs fname fargs exprTypes
    Var vname -> (Var vname,) <$> getVarType vname



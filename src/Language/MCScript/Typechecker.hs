{-#LANGUAGE NoImplicitPrelude, ConstraintKinds, DataKinds, LambdaCase#-}
module Language.MCScript.Typechecker where

import Language.MCScript.Prelude
import Language.MCScript.Types

import Polysemy
import Polysemy.State as P
import Polysemy.Error as P

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


type TypecheckC r = Members [P.State TCState, P.Error TypeError] r

getVarType :: (TypecheckC r) => Name -> Sem r Type
getVarType varName = gets varTypes <&> lookup varName >>= \case
    Nothing -> P.throw (VarDoesNotExist varName)
    Just t -> pure t

getFunReturnType :: (TypecheckC r) => Name -> Sem r Type
getFunReturnType funName = gets funReturnTypes <&> lookup funName >>= \case
    Nothing -> P.throw (FunctionDoesNotExist funName)
    Just t -> pure t

getFunArgs :: (TypecheckC r) => Name -> Sem r [Type]
getFunArgs funName = gets funArgs <&> lookup funName >>= \case
    Nothing -> P.throw (FunctionDoesNotExist funName)
    Just as -> pure as

insertFunArgs :: (TypecheckC r) => Name -> [Type] -> Sem r ()
insertFunArgs funName ts =void $ modify (\s -> s{funArgs=funArgs s & insert funName ts})

insertFunReturnType :: (TypecheckC r) => Name -> Type -> Sem r ()
insertFunReturnType funName t = void $ modify (\s -> s{varTypes=funReturnTypes s & insert funName t})

insertVarType :: (TypecheckC r) => Name -> Type -> Sem r ()
insertVarType varName t = void $ modify (\s -> s{varTypes=varTypes s & insert varName t})

typecheckModule :: (TypecheckC r) => Module -> Sem r ()
typecheckModule (Module _ instrs) = traverse_ typecheck instrs 

typecheck :: (TypecheckC r) => Statement -> Sem r ()
typecheck = \case
    CallVoid fname exprs -> do
       fargs <- getFunArgs fname
       exprTypes <- traverse typeOf exprs
       when (exprTypes /= fargs) $ P.throw $ MisMatchedFunArgs fname fargs exprTypes
    CallFun fname exprs -> void $ typeOf (FCall fname exprs)
    DefVoid fname args stmnts -> do
        insertFunArgs fname (map snd args)
        for_ args (uncurry insertVarType)
        traverse_ typecheck stmnts
    DefFun fname args stmnts lastexpr t -> do
        insertFunArgs fname (map snd args)
        for_ args (uncurry insertVarType)
        traverse_ typecheck stmnts
        retType <- typeOf lastexpr
        if (retType == t)
        then insertFunReturnType fname t
        else P.throw (MisMatchedReturnType fname t retType)
    Decl vname t expr -> do
        exprT <- typeOf expr
        if (exprT == t)
        then insertVarType vname t
        else P.throw (WrongDeclType vname t exprT)
    Assign vname expr -> do
        varT <- getVarType vname
        exprT <- typeOf expr
        when (varT /= exprT) $ P.throw (WrongAssignType vname varT exprT)
    While cond stmnts -> do
        void $ typeOf cond
        traverse_ typecheck stmnts
    DefStruct name fields -> pass -- TODO: Add to state map

typeOf :: (TypecheckC r) => Expr -> Sem r Type
typeOf = \case
    IntLit _ -> pure IntT
    FloatLit _ -> pure FloatT
    BoolLit _ -> pure BoolT
    FCall fname exprs -> do
        fargs <- getFunArgs fname
        exprTypes <- traverse typeOf exprs
        if (exprTypes == fargs)
        then getFunReturnType fname
        else P.throw $ MisMatchedFunArgs fname fargs exprTypes
    Var vname -> getVarType vname



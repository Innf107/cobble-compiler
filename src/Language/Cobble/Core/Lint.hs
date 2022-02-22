module Language.Cobble.Core.Lint where

import Language.Cobble.Prelude
import Language.Cobble.Types.QualifiedName
import Language.Cobble.Core.Types

data CoreLintError = VarNotFound QualifiedName LintEnv
                   | DeclTypeMismatch QualifiedName Type Type
                   | AppOfNonFunction 
                   deriving (Show, Eq)

data LintEnv = LintEnv {
    varTypes :: Map QualifiedName Type
} deriving (Show, Eq)

insertType :: QualifiedName -> Type -> LintEnv -> LintEnv
insertType x t = LintEnv . insert x t . varTypes

lookupType :: Members '[Error CoreLintError] r 
           => QualifiedName 
           -> LintEnv 
           -> Sem r Type
lookupType x env = case lookup x (varTypes env) of
    Nothing -> throw (VarNotFound x env)
    Just ty -> pure ty

lint :: Members '[Error CoreLintError] r
     => LintEnv
     -> [Decl]
     -> Sem r ()
lint _ [] = pure ()
lint env (Def x ty e : ds) = do
    eTy <- lintExpr env e 
    when (eTy /= ty) $ throw (DeclTypeMismatch x ty eTy)
    lint (insertType x ty env) ds

lintExpr :: Members '[Error CoreLintError] r
         => LintEnv
         -> Expr
         -> Sem r Type
lintExpr env (Var x) = lookupType x env
lintExpr env (Abs x domTy body) = do
    bodyTy <- lintExpr (insertType x domTy env) body
    pure (TFun domTy bodyTy)
lintExpr env (App e1 e2) = do
    e1Ty <- lintExpr env e1
    e2Ty <- lintExpr env e2
    case e1 of
        TFun dom codom -> 
        ty -> 


{-# LANGUAGE NoImplicitPrelude #-}
module Language.MCScript.Types where

import Language.MCScript.Prelude

type Name = Text

-- Top level module. 
data Module = Module Name [Statement] deriving (Show, Eq)

-- typecheck:
-- typecheck (Call name exprs) = if length(exprs) /= length(lookupArgs name) && sameTypes expr (lookupArgs name)
--                               then typeError: Wrong arg types
--                               else pass
-- typecheck (Deffun name args stmnts lastexpr type) = traverse_ typecheck stmnts
--                                         >> if type(lastexpr) == type
--                                            then pass
--                                            else typeerror: wrong return type
-- typecheck (Decl name type expr) = if type(expr) == type then pass else: typeError: wrong assignment type
-- typecheck (Assign name expr) = type(expr) == lookupReturnType name
-- typecheck (While init cond inc stmnts) = traverse_ type [init, cond, inc] >> traverse_ typecheck stmnts
-- typecheck (DefStruct name fields = pass
data Statement = CallVoid Name [Expr]
               | CallFun Name [Expr]
               | DefVoid Name [(Name, Type)] [Statement]
               | DefFun Name [(Name, Type)] [Statement] Expr Type
--                                                      ^ last expr
               | Decl Name Type Expr
               | Assign Name Expr
               | While Expr [Statement]
              --  | Return Expr   -- potentially very hard to implement?
               | DefStruct Name [(Name, Type)]
               deriving (Show, Eq)

data TypedStatement = TCallVoid Name [TypedExpr]
                    | TCallFun Name [TypedExpr]
                    | TDefVoid Name [(Name, Type)] [TypedStatement]
                    | TDefFun Name [(Name, Type)] [TypedStatement] TypedExpr Type
                    | TDecl Name Type Expr
                    | TAssign Name TypedExpr Type
                    | TWhile TypedExpr [TypedStatement]
                    | TDefStruct Name [(Name, Type)]
                    deriving (Show, Eq)

-- typecheck:
-- type(IntLit _) = IntT
-- type(FloatLit _) = FloatT
-- type(FCall name exprs) = if length(exprs) /= length(lookupArgs name) && sameTypes expr (lookupArgs name)
--                          then typeError: Wrong arg types
--                          else lookupReturnType name

data Expr = FCall Name [Expr]
          | IntLit Int
          | FloatLit Double
          | BoolLit Bool
          | Var Name
          deriving (Show, Eq)

data TypedExpr = TFCall Name [TypedExpr] Type
               | TIntLit Int
               | TFloatLit Double
               | TBoolLit Bool
               | TVar Name Type
               deriving (Show, Eq)

data Type = IntT | FloatT | BoolT | EntityT | StructT Name [Type] deriving (Show, Eq)

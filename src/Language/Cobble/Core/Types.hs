module Language.Cobble.Core.Types where

import Language.Cobble.Prelude
import Language.Cobble.Types.QualifiedName

data Decl = Def QualifiedName Type Expr deriving (Show, Eq, Generic, Data)

data Expr = Var QualifiedName
          | App Expr Expr
          | TyApp Expr Type
          | Abs QualifiedName Type Expr
          | TyAbs QualifiedName Kind Expr

          | IntLit Int
          | UnitLit -- Unit should ideally just be a regular variant constructor, but hard-wiring libraries into the compiler is not really possible yet.
          | Let QualifiedName Type Expr Expr
          | If Expr Expr Expr
          deriving (Show, Eq, Generic, Data)

data Type = TVar QualifiedName Kind
          | TCon QualifiedName Kind
          | TFun Type Type
          | TApp Type Type
          | TForall QualifiedName Kind Type
          deriving (Show, Eq, Generic, Data)

data Kind = KType
          | KFun Kind Kind
          deriving (Show, Eq, Generic, Data)

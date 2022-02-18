module Language.Cobble.Core.Types where

import Language.Cobble.Prelude
import Language.Cobble.Types.QualifiedName

data Expr = Var QualifiedName
          | App Expr Expr
          | TyApp Expr Type
          | Abs QualifiedName Type Expr
          | TyAbs QualifiedName Kind Expr
          deriving (Show, Eq, Generic, Data)

data Type = TVar QualifiedName Kind
          | TCon QualifiedName Kind
          | TFun Type Type
          | TForall QualifiedName Kind Type
          deriving (Show, Eq, Generic, Data)

data Kind = KType
          | KFun Kind Kind
          deriving (Show, Eq, Generic, Data)

module Language.Cobble.Core.Types where

import Language.Cobble.Prelude
import Language.Cobble.Types.QualifiedName

import Prettyprinter

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

ppQName :: QualifiedName -> Doc ann
ppQName = pretty . renderDebug

instance {-# OVERLAPPING #-} Pretty [Decl] where
    pretty ds = vsep (map pretty ds)

instance Pretty Decl where
    pretty (Def x ty e) = ppQName x <+> ":" <+> pretty ty <> line <> ppQName x <+> "=" <+> align (pretty e)
instance Pretty Expr where
    pretty (Var x) = ppQName x
    pretty (App e1 e2) = "(" <> pretty e1 <+> pretty e2 <> ")"
    pretty (TyAbs tv k e) = "(Λ(" <> ppQName tv <+> ":" <+> pretty k <> ")." <+> align (pretty e) <> ")"
    pretty (TyApp e ty) = "(" <> pretty e <+> "@" <> pretty ty <> ")"
    pretty (Abs x ty e) = "(λ(" <> ppQName x <+> ":" <+> pretty ty <> ")." <+> align (pretty e) <> ")"
    pretty (IntLit n) = pretty n
    pretty UnitLit = "()"
    pretty (Let x ty e1 e2) = "(let" <+> ppQName x <+> ":" <+> pretty ty <+> "=" <+> pretty e1 <+> "in" <+> pretty e2 <+> ")"
    pretty (If c th el) = "(if" <+> pretty c <+> "then" <+> pretty th <+> "else" <+> pretty el <> ")"
instance Pretty Type where
    pretty (TVar x k) = ppQName x
    pretty (TCon x k) = "(" <> ppQName x <+> ":" <+> pretty k <> ")"
    pretty (TFun t1 t2) = "(" <> pretty t1 <+> "->" <+> pretty t2 <> ")"
    pretty (TApp t1 t2) = "(" <> pretty t1 <+> pretty t2 <> ")"
    pretty (TForall x k ty) = "(forall" <+> "(" <> ppQName x <+> ":" <+> pretty k <> ")." <+> pretty ty <> ")"
instance Pretty Kind where
    pretty (KType) = "*"
    pretty (KFun KType t2) = "*" <+> "->" <+> pretty t2
    pretty (KFun t1 t2) = "(" <> pretty t1 <> ")" <+> "->" <+> pretty t2


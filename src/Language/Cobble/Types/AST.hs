{-# LANGUAGE NoImplicitPrelude, DataKinds, TypeFamilies, StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE GADTs, RankNTypes, PatternSynonyms, TemplateHaskell#-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Language.Cobble.Types.AST where

import Language.Cobble.Prelude

import Language.Cobble.Types.TH

type Name = Text

-- Top level module.
data Module (t :: Pass) = Module Name [Statement t] --deriving (Show, Eq)

-- | A data kind representing the state of the AST at a certain Compiler pass.
data Pass = ExpandMacros
          -- TODO: | QualifyNames
          | Typecheck
          | Codegen
          deriving (Show, Eq)


data Statement (p :: Pass) =
      CallFun (XCallFun p) LexInfo Name [Expr p]
    | DefVoid (XDefVoid p) LexInfo Name [(Name, TypeInfo p)] [Statement p]
    | DefFun  (XDefFun p) LexInfo Name [(Name, TypeInfo p)] [Statement p] (Expr p) (TypeInfo p)
    --  | DefMacro (XDefMacro p) LexInfo Name [()]
--                                                          ^ last expr
    | Decl  (XDecl p) LexInfo Name (Maybe (TypeInfo p)) (Expr p)
    | Assign (XAssign p) LexInfo Name (Expr p)
    | While (XWhile p) LexInfo (Expr p) [Statement p]
    | DefStruct (XDefStruct p) LexInfo Name [(Name, TypeInfo p)]
    | StatementX (XStatement p) LexInfo


type family XCallFun (p :: Pass)
type family XDefVoid (p :: Pass)
type family XDefFun (p :: Pass)
type family XDecl (p :: Pass)
type family XAssign (p :: Pass)
type family XWhile (p :: Pass)
type family XDefStruct (p :: Pass)
type family XStatement (p :: Pass)


data Expr (p :: Pass) =
      FCall (XFCall p) LexInfo Name [Expr p]
    | IntLit (XIntLit p) LexInfo Int
          --  | FloatLit Double Text TODO: Needs Standard Library (Postfixes?)
    | BoolLit (XBoolLit p) LexInfo Bool
    | Var (XVar p) LexInfo Name
    | ExprX (XExpr p) LexInfo

type family XFCall (p :: Pass)
type family XIntLit (p :: Pass)
type family XBoolLit (p :: Pass)
type family XVar (p :: Pass)
type family XExpr (p :: Pass)

type family TypeInfo (p :: Pass)

data Type = IntT | BoolT | EntityT | StructT Name deriving (Show, Eq)

type FileName = Text


data LexInfo = LexInfo {
      line :: Int
    , column :: Int
    , file :: FileName
    } deriving (Show, Eq)

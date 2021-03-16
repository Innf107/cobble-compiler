{-# LANGUAGE NoImplicitPrelude, DataKinds, TypeFamilies, StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE GADTs, RankNTypes, PatternSynonyms, TemplateHaskell#-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Cobble.Types.AST where

import Language.Cobble.Prelude
import Language.Cobble.Util.Convert

import Language.Cobble.Shared

import Language.Cobble.Types.TH

import Language.Cobble.MCAsm.McFunction

type family Name (p :: Pass)

-- Top level module.
data Module (p :: Pass) = Module (Name p) [Statement p] --deriving (Show, Eq)

-- | A data kind representing the state of the AST at a certain Compiler pass.
data Pass = QualifyNames
          | Typecheck
          | Codegen
          deriving (Show, Eq)


data Statement (p :: Pass) =
      CallFun (XCallFun p) LexInfo (Name p) [Expr p]
    | DefVoid (XDefVoid p) LexInfo (Name p) [(Name p, Type p)] [Statement p]
    | DefFun  (XDefFun p)  LexInfo (Name p) [(Name p, Type p)] [Statement p] (Expr p) (Type p)
--                                                          ^ last expr
    | Decl (XDecl p) LexInfo (Name p) (Maybe (Type p)) (Expr p)
    | Assign (XAssign p) LexInfo (Name p) (Expr p)
    | While (XWhile p) LexInfo (Expr p) [Statement p]
    | DefStruct (XDefStruct p) LexInfo (Name p) [(Name p, Type p)]
    | SetScoreboard (XSetScoreboard p) LexInfo Text Text (Expr p)
--                                    objective^    ^player
    | StatementX (XStatement p) LexInfo


type family XCallFun (p :: Pass)
type family XDefVoid (p :: Pass)
type family XDefFun (p :: Pass)
type family XDecl (p :: Pass)
type family XAssign (p :: Pass)
type family XWhile (p :: Pass)
type family XDefStruct (p :: Pass)
type family XSetScoreboard (p :: Pass)
type family XStatement (p :: Pass)


data Expr (p :: Pass) =
      FCall (XFCall p) LexInfo (Name p) [Expr p]
    | IntLit (XIntLit p) LexInfo Int
          --  | FloatLit Double Text TODO: Needs Standard Library (Postfixes?)
    | BoolLit (XBoolLit p) LexInfo Bool
    | Var (XVar p) LexInfo (Name p)
    | ExprX (XExpr p) LexInfo

type family XFCall (p :: Pass)
type family XIntLit (p :: Pass)
type family XBoolLit (p :: Pass)
type family XVar (p :: Pass)
type family XExpr (p :: Pass)

data Type (p :: Pass) = IntT | BoolT | EntityT | StructT (Name p)

type FileName = Text


data LexInfo = LexInfo {
      line :: Int
    , column :: Int
    , file :: FileName
    } deriving (Show, Eq)

instance (Name p1 ~ Name p2) => Convert (Type p1) (Type p2) where
    conv = \case
        IntT -> IntT
        BoolT -> BoolT
        EntityT -> EntityT
        StructT n -> StructT n

{-#OPTIONS_GHC -Wno-orphans#-}
{-#LANGUAGE NoImplicitPrelude, DataKinds, TemplateHaskell#-}
{-# LANGUAGE PatternSynonyms, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Cobble.Types.AST.ExpandMacros where

import Language.Cobble.Prelude
import Language.Cobble.Types.AST
import Language.Cobble.Types.TH
  
deriving instance Show (Statement 'ExpandMacros)
deriving instance Eq (Statement 'ExpandMacros)
  
deriving instance Show (Expr 'ExpandMacros)
deriving instance Eq (Expr 'ExpandMacros)
  
deriving instance Show (Type 'ExpandMacros)
deriving instance Eq (Type 'ExpandMacros)
  
type instance XCallFun 'ExpandMacros = ()
type instance XDefFun 'ExpandMacros = ()
type instance XDefVoid 'ExpandMacros = ()
type instance XDecl 'ExpandMacros = ()
type instance XAssign 'ExpandMacros = ()
type instance XWhile 'ExpandMacros = ()
type instance XDefStruct 'ExpandMacros = ()
type instance XStatement 'ExpandMacros = CallStatementMacroX

data CallStatementMacroX = CallStatementMacroX (Name 'ExpandMacros) [MacroParam] deriving (Show, Eq)

data MacroParam = PStatement (Statement 'ExpandMacros) 
                | PExpr (Expr 'ExpandMacros) 
                | PStatements [Statement 'ExpandMacros]
                deriving (Show, Eq) 

pattern CallStatementMacro :: LexInfo -> Name 'ExpandMacros -> [MacroParam] -> Statement 'ExpandMacros
pattern CallStatementMacro l n ps <- StatementX (CallStatementMacroX n ps) l
    where
        CallStatementMacro l n ps = StatementX (CallStatementMacroX n ps) l

type instance XFCall 'ExpandMacros = ()
type instance XIntLit 'ExpandMacros = ()
type instance XBoolLit 'ExpandMacros = ()
type instance XVar 'ExpandMacros = ()
type instance XExpr 'ExpandMacros = () -- TODO: Expression Macro?

type instance Name 'ExpandMacros = Text
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.Typecheck where

import Language.Cobble.Prelude

import Data.Data
import Data.Generics.Uniplate.Data

import Language.Cobble.Types.AST
import Language.Cobble.Types.TH
import Language.Cobble.Shared
  
-- Needed for @ModSig@ instances
import Language.Cobble.Types.AST.Codegen
  
deriving instance Show     (Module 'Typecheck)
deriving instance Eq       (Module 'Typecheck)
deriving instance Generic  (Module 'Typecheck)
deriving instance Data     (Module 'Typecheck)
deriving instance Typeable (Module 'Typecheck)

deriving instance Show    (Statement 'Typecheck)
deriving instance Eq      (Statement 'Typecheck)
deriving instance Generic (Statement 'Typecheck)
deriving instance Data    (Statement 'Typecheck)

deriving instance Show    (Expr 'Typecheck)
deriving instance Eq      (Expr 'Typecheck)
deriving instance Generic (Expr 'Typecheck)
deriving instance Data    (Expr 'Typecheck)

deriving instance Show    (Type 'Typecheck)
deriving instance Eq      (Type 'Typecheck)
deriving instance Generic (Type 'Typecheck)
deriving instance Data    (Type 'Typecheck)

type instance XModule 'Typecheck = Map (Name 'Codegen) ModSig
  
type instance XDef           'Typecheck = ()
type instance XImport        'Typecheck = ()
type instance XDefStruct     'Typecheck = ()
type instance XStatement     'Typecheck = Void

type instance XFCall   'Typecheck = ()
type instance XIntLit  'Typecheck = ()
type instance XBoolLit 'Typecheck = ()
type instance XIf      'Typecheck = (QualifiedName, Int)
type instance XLet     'Typecheck = ()
type instance XVar     'Typecheck = ()
type instance XExpr    'Typecheck = Void

type instance Name 'Typecheck = QualifiedName

type instance XKind 'Typecheck = Kind

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.Typecheck where

import Language.Cobble.Prelude

import Data.Data
import Data.Generics.Uniplate.Data

import Language.Cobble.Types.AST
import Language.Cobble.Shared

type instance XModule 'Typecheck = Ext Typecheck (Map (Name 'Codegen) ModSig)
  
type instance XDef           'Typecheck = IgnoreExt Typecheck
type instance XDecl          'Typecheck = IgnoreExt Typecheck
type instance XParam         'Typecheck = Ext Typecheck [Name 'Typecheck]
type instance XImport        'Typecheck = IgnoreExt Typecheck
type instance XDefStruct     'Typecheck = IgnoreExt Typecheck
type instance XStatement     'Typecheck = ExtVoid Typecheck

type instance XFCall            'Typecheck = IgnoreExt Typecheck
type instance XIntLit           'Typecheck = IgnoreExt Typecheck
type instance XIf               'Typecheck = Ext Typecheck (QualifiedName, Int)
type instance XLet              'Typecheck = IgnoreExt Typecheck
type instance XVar              'Typecheck = IgnoreExt Typecheck
type instance XStructConstruct  'Typecheck = StructDef Typecheck
type instance XStructAccess     'Typecheck = Map (Name Typecheck) (StructDef Typecheck)
type instance XExpr             'Typecheck = ExtVoid Typecheck

type instance Name 'Typecheck = QualifiedName

type instance XKind 'Typecheck = Kind

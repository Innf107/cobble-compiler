{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.Typecheck where

import Language.Cobble.Prelude

import Data.Data
import Data.Generics.Uniplate.Data

import Language.Cobble.Types.AST
import Language.Cobble.Shared

type instance XModule 'Typecheck = Map (Name 'Codegen) ModSig
  
type instance XDef           'Typecheck = ()
type instance XDecl          'Typecheck = ()
type instance XParam         'Typecheck = [Name 'Codegen]
type instance XImport        'Typecheck = ()
type instance XDefStruct     'Typecheck = ()
type instance XStatement     'Typecheck = Void

type instance XFCall            'Typecheck = ()
type instance XIntLit           'Typecheck = ()
type instance XIf               'Typecheck = (QualifiedName, Int)
type instance XLet              'Typecheck = ()
type instance XVar              'Typecheck = ()
type instance XStructConstruct  'Typecheck = StructDef Typecheck
type instance XExpr             'Typecheck = Void

type instance Name 'Typecheck = QualifiedName

type instance XKind 'Typecheck = Kind

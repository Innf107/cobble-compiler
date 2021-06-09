{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.SemAnalysis where

import Language.Cobble.Prelude

import Data.Data
import Data.Generics.Uniplate.Data

import Language.Cobble.Types.AST
import Language.Cobble.Shared

type instance XModule 'SemAnalysis = Map (Name 'Codegen) ModSig
  
type instance XDef           'SemAnalysis = ()
type instance XDecl          'SemAnalysis = ()
type instance XParam         'SemAnalysis = [Name 'Codegen]
type instance XImport        'SemAnalysis = ()
type instance XDefStruct     'SemAnalysis = ()
type instance XStatement     'SemAnalysis = Void

type instance XFCall            'SemAnalysis = ()
type instance XIntLit           'SemAnalysis = ()
type instance XIf               'SemAnalysis = (QualifiedName, Int)
type instance XLet              'SemAnalysis = ()
type instance XVar              'SemAnalysis = ()
type instance XStructConstruct  'SemAnalysis = StructDef 'SemAnalysis
type instance XExpr             'SemAnalysis = Void

type instance Name 'SemAnalysis = QualifiedName

type instance XKind 'SemAnalysis = Kind

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.SemAnalysis where

import Language.Cobble.Prelude

import Data.Data
import Data.Generics.Uniplate.Data

import Language.Cobble.Types.AST
import Language.Cobble.Shared

type instance XModule 'SemAnalysis = Ext SemAnalysis (Map (Name 'Codegen) ModSig)
  
type instance XDef           'SemAnalysis = IgnoreExt SemAnalysis
type instance XDecl          'SemAnalysis = IgnoreExt SemAnalysis
type instance XParam         'SemAnalysis = Ext SemAnalysis [Name 'Codegen]
type instance XImport        'SemAnalysis = IgnoreExt SemAnalysis
type instance XDefStruct     'SemAnalysis = IgnoreExt SemAnalysis
type instance XStatement     'SemAnalysis = ExtVoid SemAnalysis

type instance XFCall            'SemAnalysis = IgnoreExt SemAnalysis
type instance XIntLit           'SemAnalysis = IgnoreExt SemAnalysis
type instance XIf               'SemAnalysis = Ext SemAnalysis (QualifiedName, Int)
type instance XLet              'SemAnalysis = IgnoreExt SemAnalysis
type instance XVar              'SemAnalysis = IgnoreExt SemAnalysis
type instance XStructConstruct  'SemAnalysis = StructDef SemAnalysis
type instance XStructAccess     'SemAnalysis = Map (Name SemAnalysis) (StructDef SemAnalysis)
type instance XExpr             'SemAnalysis = ExtVoid SemAnalysis

type instance Name 'SemAnalysis = QualifiedName

type instance XKind 'SemAnalysis = Kind

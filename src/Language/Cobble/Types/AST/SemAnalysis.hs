{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.SemAnalysis where

import Language.Cobble.Prelude

import Data.Data
import Data.Generics.Uniplate.Data

import Language.Cobble.Types.AST
import Language.Cobble.Types.QualifiedName

type instance XModule 'SemAnalysis = (Map (Name 'Codegen) ModSig)
  
type instance XDef              SemAnalysis = IgnoreExt SemAnalysis
type instance XDecl             SemAnalysis = IgnoreExt SemAnalysis
type instance XParam            SemAnalysis = [Name 'Codegen]
type instance XImport           SemAnalysis = IgnoreExt SemAnalysis
type instance XDefStruct        SemAnalysis = Kind
type instance XDefVariant       SemAnalysis = Kind
type instance XDefVariantClause SemAnalysis = (Int, Int)
type instance XDefClass         SemAnalysis = Kind
-- XDefInstance uses a list of pairs instead of a Map, because SemAnalysis shuffles declarations around
-- to have the same order as class declaration.
type instance XDefInstance      SemAnalysis = ([(QualifiedName, Type Codegen)], [TVar Codegen])
type instance XStatement        SemAnalysis = ExtVoid SemAnalysis

type instance XFCall            'SemAnalysis = IgnoreExt SemAnalysis
type instance XIntLit           'SemAnalysis = IgnoreExt SemAnalysis
type instance XIf               'SemAnalysis = IgnoreExt SemAnalysis
type instance XLet              'SemAnalysis = IgnoreExt SemAnalysis
type instance XVar              'SemAnalysis = IgnoreExt SemAnalysis
type instance XAscription       'SemAnalysis = IgnoreExt SemAnalysis
type instance XVariantConstr    'SemAnalysis = (Int, Int)
--                                               ^    ^
--                                               |    constructor index
--                                               expected number of args
-- | @StructDef@ has its own @CoercePass@ instance, so we don't need @Ext@ here 
type instance XCase             'SemAnalysis = IgnoreExt SemAnalysis
type instance XStructConstruct  'SemAnalysis = StructDef SemAnalysis
-- | @Map@ also has its own @CoercePass@ instance, so we don't need @Ext@ here
type instance XStructAccess     'SemAnalysis = Map (Name SemAnalysis) (StructDef SemAnalysis)
type instance XExpr             'SemAnalysis = ExtVoid SemAnalysis

type instance XCaseBranch SemAnalysis = IgnoreExt SemAnalysis

type instance XIntP     SemAnalysis = IgnoreExt SemAnalysis
type instance XVarP     SemAnalysis = IgnoreExt SemAnalysis
type instance XConstrP  SemAnalysis = IgnoreExt SemAnalysis

type instance Name 'SemAnalysis = QualifiedName

type instance XKind 'SemAnalysis = Kind

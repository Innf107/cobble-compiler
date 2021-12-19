{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.SemAnalysis where

import Language.Cobble.Prelude

import Data.Data hiding (Fixity)
import Data.Generics.Uniplate.Data

import Language.Cobble.Types.AST
import Language.Cobble.Types.QualifiedName

type instance XModule 'SemAnalysis = (Map (Name 'Codegen) ModSig)
  
type instance XDef              SemAnalysis = Maybe Fixity
type instance XDecl             SemAnalysis = ()
type instance XParam            SemAnalysis = [Name 'Codegen]
type instance XImport           SemAnalysis = ()
type instance XDefStruct        SemAnalysis = Kind
type instance XDefVariant       SemAnalysis = Kind
type instance XDefVariantClause SemAnalysis = (Int, Int)
type instance XDefClass         SemAnalysis = Kind
-- XDefInstance uses a list of pairs instead of a Map, because SemAnalysis shuffles declarations around
-- to have the same order as class declaration.
type instance XDefInstance      SemAnalysis = ([(QualifiedName, Type Codegen)], [TVar Codegen])
type instance XStatement        SemAnalysis = Void

type instance XFCall            'SemAnalysis = ()
type instance XIntLit           'SemAnalysis = ()
type instance XIf               'SemAnalysis = ()
type instance XLet              'SemAnalysis = ()
type instance XVar              'SemAnalysis = ()
type instance XAscription       'SemAnalysis = ()
type instance XVariantConstr    'SemAnalysis = (Int, Int)
--                                               ^    ^
--                                               |    constructor index
--                                               expected number of args
-- | @StructDef@ has its own @CoercePass@ instance, so we don't need @Ext@ here 
type instance XCase             'SemAnalysis = ()
type instance XStructConstruct  'SemAnalysis = StructDef SemAnalysis
-- | @Map@ also has its own @CoercePass@ instance, so we don't need @Ext@ here
type instance XStructAccess     'SemAnalysis = Map (Name SemAnalysis) (StructDef SemAnalysis)
type instance XExpr             'SemAnalysis = Void

type instance XCaseBranch SemAnalysis = ()

type instance XIntP     SemAnalysis = ()
type instance XVarP     SemAnalysis = ()
type instance XConstrP  SemAnalysis = Int
type instance XPattern  SemAnalysis = Void

type instance Name 'SemAnalysis = QualifiedName

type instance XKind 'SemAnalysis = Kind

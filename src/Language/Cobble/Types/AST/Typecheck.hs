{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.Typecheck where

import Language.Cobble.Prelude

import Data.Data
import Data.Generics.Uniplate.Data

import Language.Cobble.Types.AST
import Language.Cobble.Types.QualifiedName

type instance XModule Typecheck = Map (Name Codegen) ModSig
  
type instance XDef              Typecheck = IgnoreExt Typecheck
type instance XDecl             Typecheck = IgnoreExt Typecheck
type instance XParam            Typecheck = [Name Typecheck]
type instance XImport           Typecheck = IgnoreExt Typecheck
type instance XDefStruct        Typecheck = Kind
type instance XDefVariant       Typecheck = Kind
type instance XDefVariantClause Typecheck = (Int, Int)
type instance XDefClass         Typecheck = Kind
-- XDefInstance uses a list of pairs instead of a Map, because SemAnalysis shuffles declarations around
-- to have the same order as class declaration.
type instance XDefInstance      Typecheck = ([(QualifiedName, Type Codegen)], [TVar Codegen])
type instance XStatement        Typecheck = ExtVoid Typecheck

type instance XFCall            Typecheck = IgnoreExt Typecheck
type instance XIntLit           Typecheck = IgnoreExt Typecheck
type instance XIf               Typecheck = IgnoreExt Typecheck
type instance XLet              Typecheck = IgnoreExt Typecheck
type instance XVar              Typecheck = IgnoreExt Typecheck
type instance XAscription       Typecheck = IgnoreExt Typecheck
type instance XVariantConstr    Typecheck = (Int, Int)
--                                                           ^    ^
--                                                           |    constructor index
--                                                           expected number of args
type instance XCase             Typecheck = IgnoreExt Typecheck
type instance XStructConstruct  Typecheck = StructDef Typecheck
type instance XStructAccess     Typecheck = Map (Name Typecheck) (StructDef Typecheck)
type instance XExpr             Typecheck = ExtVoid Typecheck

type instance XCaseBranch Typecheck = IgnoreExt Typecheck

type instance XIntP     Typecheck = IgnoreExt Typecheck
type instance XVarP     Typecheck = IgnoreExt Typecheck
type instance XConstrP  Typecheck = IgnoreExt Typecheck

type instance Name Typecheck = QualifiedName

type instance XKind Typecheck = Kind

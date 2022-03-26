{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.Typecheck where

import Language.Cobble.Prelude

import Data.Data hiding (Fixity)
import Data.Generics.Uniplate.Data

import Language.Cobble.Types.AST
import Language.Cobble.Types.QualifiedName

type instance XModule Typecheck = Map (Name Codegen) ModSig
  
type instance XDef              Typecheck = Maybe Fixity
type instance XDecl             Typecheck = ()
type instance XParam            Typecheck = [Name Typecheck]
type instance XImport           Typecheck = ()
type instance XDefStruct        Typecheck = Kind
type instance XDefVariant       Typecheck = Kind
type instance XDefVariantClause Typecheck = (Int, Int)
type instance XDefClass         Typecheck = Kind
-- XDefInstance uses a list of pairs instead of a Map, because SemAnalysis shuffles declarations around
-- to have the same order as class declaration.
type instance XDefInstance      Typecheck = ([(QualifiedName, Type)], [TVar])
type instance XStatement        Typecheck = Void

type instance XFCall            Typecheck = ()
type instance XIntLit           Typecheck = ()
type instance XIf               Typecheck = ()
type instance XLet              Typecheck = ()
type instance XVar              Typecheck = ()
type instance XAscription       Typecheck = ()
type instance XVariantConstr    Typecheck = (Int, Int)
--                                                           ^    ^
--                                                           |    constructor index
--                                                           expected number of args
type instance XCase             Typecheck = ()
type instance XStructConstruct  Typecheck = StructDef
type instance XStructAccess     Typecheck = Map (Name Typecheck) StructDef

type instance XLambda           Typecheck = ()

type instance XExpr             Typecheck = Void

type instance XCaseBranch Typecheck = ()

type instance XIntP         Typecheck = ()
type instance XVarP         Typecheck = ()
type instance XConstrP      Typecheck = (Int, TypeVariant)
                                  --     ^constr index
type instance XWildcardP    Typecheck = ()
type instance XPattern      Typecheck = Void

type instance Name Typecheck = QualifiedName

type instance XKind Typecheck = Kind

type instance XType Typecheck = Type
type instance XTVar Typecheck = TVar


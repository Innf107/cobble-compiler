{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Cobble.Syntax.AST.Typecheck where

import Cobble.Prelude

import Data.Data hiding (Fixity)
import Data.Generics.Uniplate.Data

import Cobble.Syntax.AST
import Cobble.Syntax.QualifiedName

type instance XModule Typecheck = Map Text ModSig
  
type instance XDef              Typecheck = Maybe Fixity
type instance XDecl             Typecheck = ()
type instance XParam            Typecheck = Seq (Name Typecheck)
type instance XImport           Typecheck = Void
type instance XDefVariant       Typecheck = Kind
type instance XDefVariantClause Typecheck = (Int, Int)
type instance XDefEffect        Typecheck = Kind
type instance XDefClass         Typecheck = Kind
-- XDefInstance uses a list of pairs instead of a Map, because SemAnalysis shuffles declarations around
-- to have the same order as class declaration.
type instance XDefInstance      Typecheck = (Kind, Seq (QualifiedName, Type), Seq TVar, Bool)
--                                                                                      ^ is imported
type instance XStatement        Typecheck = Void

type instance XFCall            Typecheck = ()
type instance XIntLit           Typecheck = ()
type instance XIf               Typecheck = ()
type instance XLet              Typecheck = ()
type instance XVar              Typecheck = ()
type instance XAscription       Typecheck = ()
type instance XVariantConstr    Typecheck = (Int, Int)
--                                           ^    ^
--                                           |    constructor index
--                                           expected number of args
type instance XCase             Typecheck = ()

type instance XLambda           Typecheck = ()

type instance XHandle           Typecheck = ()
type instance XResume           Typecheck = ()

type instance XExpr             Typecheck = Void

type instance XCaseBranch Typecheck = ()

type instance XEffHandler Typecheck = ()

type instance XIntP         Typecheck = ()
type instance XVarP         Typecheck = ()
type instance XConstrP      Typecheck = (Int, TypeVariant)
                                  --     ^constr index
type instance XWildcardP    Typecheck = ()
type instance XOrP          Typecheck = ()
type instance XPattern      Typecheck = Void

type instance Name Typecheck = QualifiedName

type instance XKind Typecheck = Kind

type instance XType Typecheck = Type
type instance XTVar Typecheck = TVar


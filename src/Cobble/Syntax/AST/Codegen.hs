{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Cobble.Syntax.AST.Codegen where
  
import Data.Data hiding (Fixity)
import Data.Generics.Uniplate.Data
  
import Cobble.Prelude

import Cobble.Syntax.AST
import Cobble.Syntax.Instances
import Cobble.Syntax.QualifiedName

type instance XModule Codegen = Map Text ModSig

type instance XDecl      Codegen = Type
type instance XParam     Codegen = (Seq (Name Codegen, Type))

type instance XDef              Codegen = Maybe Fixity
type instance XImport           Codegen = Void
type instance XDefVariant       Codegen = Kind
type instance XDefVariantClause Codegen = (Type, Int, Int)
type instance XDefEffect        Codegen = Kind
type instance XDefClass         Codegen = Kind
-- XDefInstance uses a list of pairs instead of a Map, because SemAnalysis shuffles declarations around
-- to have the same order as class declaration.
type instance XDefInstance      Codegen = (Kind, (Seq (QualifiedName, Type)), (Seq TVar), QualifiedName)
--                                                                      ^param tvs  ^ dictionary name
type instance XStatement        Codegen = Void

type instance XFCall            Codegen = Type
type instance XIntLit           Codegen = ()
type instance XIf               Codegen = Type
type instance XLet              Codegen = ()
type instance XVar              Codegen = (Type)
type instance XAscription       Codegen = Void
type instance XVariantConstr    Codegen = (Type, Type, Int)
type instance XCase             Codegen = Type


type instance XLambda            Codegen = (Type, Type, Type)
                                        -- ^      ^     ^effect type 
                                        -- |      parameter type
                                        -- full type

type instance XHandle           Codegen = (Type, Effect)
type instance XResume           Codegen = Type

type instance XExpr              Codegen = CodegenExt

type instance XCaseBranch Codegen = ()

type instance XEffHandler Codegen = Effect

type instance XIntP         Codegen = ()
type instance XVarP         Codegen = Type
type instance XConstrP      Codegen = (Type, Int, TypeVariant)
                                --     ^constr index
type instance XWildcardP    Codegen = Type
type instance XOrP          Codegen = Type
type instance XPattern      Codegen = Void

type instance Name 'Codegen = QualifiedName

type instance XKind 'Codegen = Kind

type instance XType Codegen = Type
type instance XTVar Codegen = TVar

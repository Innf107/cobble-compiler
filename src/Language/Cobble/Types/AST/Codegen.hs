{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.Codegen where
  
import Data.Data
import Data.Generics.Uniplate.Data
  
import Language.Cobble.Prelude

import Language.Cobble.Types.AST
import Language.Cobble.Types.Instances
import Language.Cobble.Types.QualifiedName

-- Ugh...
import Language.Cobble.Types.AST.PostProcess

-- Defined here because the instances need
-- @Type 'Codegen@ to be defined
deriving instance Show ModSig
deriving instance Eq ModSig
deriving instance Data ModSig

deriving instance Show TypeVariant
deriving instance Eq TypeVariant
deriving instance Data TypeVariant

type instance XModule Codegen = Map (Name Codegen) ModSig

type instance XDecl      Codegen = (Type Codegen, [TGiven])
type instance XParam     Codegen = [(Name Codegen, Type Codegen)]

type instance XDef              Codegen = ()
type instance XImport           Codegen = ()
type instance XDefStruct        Codegen = Kind
type instance XDefVariant       Codegen = Kind
type instance XDefVariantClause Codegen = (Type Codegen, Int, Int)
type instance XDefClass         Codegen = Kind
-- XDefInstance uses a list of pairs instead of a Map, because SemAnalysis shuffles declarations around
-- to have the same order as class declaration.
type instance XDefInstance      Codegen = ([(QualifiedName, Type Codegen)], [TVar Codegen])
type instance XStatement        Codegen = Void

type instance XFCall            Codegen = Type Codegen
type instance XIntLit           Codegen = ()
type instance XIf               Codegen = ()
type instance XLet              Codegen = ()
type instance XVar              Codegen = (Type Codegen, [TWanted])
type instance XAscription       Codegen = Void
type instance XVariantConstr    Codegen = (Type Codegen, Int, Int)
--                                                        ^    ^
--                                                        |    constructor index
--                                                        expected number of args
type instance XCase             Codegen = Type Codegen
type instance XStructConstruct  'Codegen = (StructDef 'Codegen, Type 'Codegen)
type instance XStructAccess     'Codegen = (StructDef 'Codegen, Type 'Codegen)
type instance XExpr             'Codegen = Void

type instance XCaseBranch Codegen = ()

type instance XIntP     Codegen = ()
type instance XVarP     Codegen = Type Codegen
type instance XConstrP  Codegen = (Type Codegen, Int)
type instance XPattern  Codegen = Void

type instance Name 'Codegen = QualifiedName

type instance XKind 'Codegen = Kind

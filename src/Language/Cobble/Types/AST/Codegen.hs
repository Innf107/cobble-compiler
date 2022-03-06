{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.Codegen where
  
import Data.Data hiding (Fixity)
import Data.Generics.Uniplate.Data
  
import Language.Cobble.Prelude

import Language.Cobble.Types.AST
import Language.Cobble.Types.Instances
import Language.Cobble.Types.QualifiedName

-- Defined here because the instances need
-- @Type 'Codegen@ to be defined
deriving instance Show ModSig
deriving instance Eq ModSig
deriving instance Data ModSig

deriving instance Show TypeVariant
deriving instance Eq TypeVariant
deriving instance Data TypeVariant

type instance XModule Codegen = Map (Name Codegen) ModSig

type instance XDecl      Codegen = (Type, [TGiven])
type instance XParam     Codegen = [(Name Codegen, Type)]

type instance XDef              Codegen = Maybe Fixity
type instance XImport           Codegen = ()
type instance XDefStruct        Codegen = Kind
type instance XDefVariant       Codegen = Kind
type instance XDefVariantClause Codegen = (Type, Int, Int)
type instance XDefClass         Codegen = Kind
-- XDefInstance uses a list of pairs instead of a Map, because SemAnalysis shuffles declarations around
-- to have the same order as class declaration.
type instance XDefInstance      Codegen = ([(QualifiedName, Type)], [TVar])
type instance XStatement        Codegen = Void

type instance XFCall            Codegen = Type
type instance XIntLit           Codegen = ()
type instance XIf               Codegen = Type
type instance XLet              Codegen = ()
type instance XVar              Codegen = (Type, [TWanted])
type instance XAscription       Codegen = Void
type instance XVariantConstr    Codegen = (Type, Int, Int)
--                                                        ^    ^
--                                                        |    constructor index
--                                                        expected number of args
type instance XCase             Codegen = Type

type instance XStructConstruct  'Codegen = (StructDef, Type)
type instance XStructAccess     'Codegen = (StructDef, Type)

type instance XLambda            Codegen = (Type, Type)
                                        -- ^      ^      
                                        -- |      parameter type
                                        -- full type

type instance XExpr              Codegen = CodegenExt

type instance XCaseBranch Codegen = ()

type instance XIntP     Codegen = ()
type instance XVarP     Codegen = Type
type instance XConstrP  Codegen = (Type, Int)
type instance XPattern  Codegen = Void

type instance Name 'Codegen = QualifiedName

type instance XKind 'Codegen = Kind

type instance XType Codegen = Type
type instance XTVar Codegen = TVar

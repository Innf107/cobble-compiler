{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Cobble.Types.AST.Codegen where
  
import Data.Data hiding (Fixity)
import Data.Generics.Uniplate.Data
  
import Cobble.Prelude

import Cobble.Types.AST
import Cobble.Types.Instances
import Cobble.Types.QualifiedName

-- Defined here because the instances need
-- @Type 'Codegen@ to be defined
deriving instance Show ModSig
deriving instance Eq ModSig
deriving instance Data ModSig

deriving instance Show TypeVariant
deriving instance Eq TypeVariant
deriving instance Data TypeVariant

type instance XModule Codegen = Map (Name Codegen) ModSig

type instance XDecl      Codegen = (Type, (Seq TGiven))
type instance XParam     Codegen = (Seq (Name Codegen, Type))

type instance XDef              Codegen = Maybe Fixity
type instance XImport           Codegen = ()
type instance XDefStruct        Codegen = Kind
type instance XDefVariant       Codegen = Kind
type instance XDefVariantClause Codegen = (Type, Int, Int)
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
type instance XVar              Codegen = (Type, (Seq TWanted))
type instance XAscription       Codegen = Void
type instance XVariantConstr    Codegen = (Type, Type, Int)
type instance XCase             Codegen = Type

type instance XStructConstruct  'Codegen = (StructDef, Type)
type instance XStructAccess     'Codegen = (StructDef, Type)

type instance XLambda            Codegen = (Type, Type)
                                        -- ^      ^      
                                        -- |      parameter type
                                        -- full type

type instance XExpr              Codegen = CodegenExt

type instance XCaseBranch Codegen = ()

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
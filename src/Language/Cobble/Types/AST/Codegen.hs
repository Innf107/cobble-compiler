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

type instance XModule Codegen = Ext Codegen (Map (Name Codegen) ModSig)

type instance XDecl      Codegen = Ext2_1 Codegen (Type Codegen) [TGiven]
type instance XParam     Codegen = Ext Codegen [(Name Codegen, Type Codegen)]

type instance XDef              Codegen = IgnoreExt Codegen
type instance XImport           Codegen = IgnoreExt Codegen
type instance XDefStruct        Codegen = Ext Codegen Kind
type instance XDefVariant       Codegen = Ext Codegen Kind
type instance XDefVariantClause Codegen = Ext3_1 Codegen (Type Codegen) Int Int
type instance XDefClass         Codegen = Ext Codegen Kind
-- XDefInstance uses a list of pairs instead of a Map, because SemAnalysis shuffles declarations around
-- to have the same order as class declaration.
type instance XDefInstance      Codegen = Ext Codegen ([(QualifiedName, Type Codegen)], [TVar Codegen])
type instance XStatement        Codegen = ExtVoid Codegen

type instance XFCall            Codegen = Ext Codegen (Type Codegen)
type instance XIntLit           Codegen = IgnoreExt Codegen
type instance XIf               Codegen = IgnoreExt Codegen
type instance XLet              Codegen = IgnoreExt Codegen
type instance XVar              Codegen = Ext2_1 Codegen (Type Codegen) [TWanted]
type instance XVariantConstr    Codegen = Ext Codegen (Type Codegen, Int, Int)
--                                                                    ^    ^
--                                                                    |    constructor index
--                                                                    expected number of args
type instance XStructConstruct  'Codegen = Ext Codegen (StructDef 'Codegen, Type 'Codegen)
type instance XStructAccess     'Codegen = Ext Codegen (StructDef 'Codegen, Type 'Codegen)
type instance XExpr             'Codegen = ExtVoid Codegen

type instance Name 'Codegen = QualifiedName

type instance XKind 'Codegen = Kind

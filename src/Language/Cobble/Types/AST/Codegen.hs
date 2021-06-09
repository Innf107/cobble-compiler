{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.Codegen where
  
import Data.Data
import Data.Generics.Uniplate.Data
  
import Language.Cobble.Prelude

import Language.Cobble.Types.AST
import Language.Cobble.Shared

-- Defined here because the instances need
-- @Type 'Codegen@ to be defined
deriving instance Show ModSig
deriving instance Eq ModSig
deriving instance Data ModSig

deriving instance Show TypeVariant
deriving instance Eq TypeVariant
deriving instance Data TypeVariant

type instance XModule 'Codegen = Map (Name 'Codegen) ModSig

type instance XDecl      'Codegen = Type 'Codegen
type instance XParam     'Codegen = [(Name 'Codegen, Type 'Codegen)]

type instance XDef       'Codegen = () -- Return Type
type instance XImport    'Codegen = ()
type instance XDefStruct 'Codegen = ()
type instance XStatement 'Codegen = Void

type instance XFCall            'Codegen = Type 'Codegen
type instance XIntLit           'Codegen = ()
type instance XIf               'Codegen = (QualifiedName, Int)
type instance XLet              'Codegen = ()
type instance XVar              'Codegen = Type 'Codegen
type instance XStructConstruct  'Codegen = (StructDef 'Codegen, Type 'Codegen)
type instance XExpr             'Codegen = Void

type instance Name 'Codegen = QualifiedName

type instance XKind 'Codegen = Kind

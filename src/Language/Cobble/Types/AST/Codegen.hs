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

deriving instance Show     (Module 'Codegen)
deriving instance Eq       (Module 'Codegen)
deriving instance Generic  (Module 'Codegen)
deriving instance Data     (Module 'Codegen)
deriving instance Typeable (Module 'Codegen)

deriving instance Show     (Statement 'Codegen)
deriving instance Eq       (Statement 'Codegen)
deriving instance Generic  (Statement 'Codegen)
deriving instance Data     (Statement 'Codegen)
deriving instance Typeable (Statement 'Codegen)

deriving instance Show     (Expr 'Codegen)
deriving instance Eq       (Expr 'Codegen)
deriving instance Generic  (Expr 'Codegen)
deriving instance Data     (Expr 'Codegen)
deriving instance Typeable (Expr 'Codegen)

deriving instance Show     (Type 'Codegen)
deriving instance Eq       (Type 'Codegen)
deriving instance Generic  (Type 'Codegen)
deriving instance Data     (Type 'Codegen)
deriving instance Typeable (Type 'Codegen)

type instance XModule 'Codegen = Map (Name 'Codegen) ModSig

type instance XDef     'Codegen = Type 'Codegen -- Return Type
type instance XParam   'Codegen = [(Name 'Codegen, Type 'Codegen)]
type instance XImport  'Codegen = ()
type instance XDefStruct 'Codegen = ()
type instance XStatement 'Codegen = Void

type instance XFCall   'Codegen = Type 'Codegen
type instance XIntLit  'Codegen = ()
type instance XIf      'Codegen = (QualifiedName, Int)
type instance XVar     'Codegen = Type 'Codegen
type instance XLet     'Codegen = ()
type instance XExpr    'Codegen = Void

type instance Name 'Codegen = QualifiedName

type instance XKind 'Codegen = Kind

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.QualifyNames where
  
import Data.Data
import Data.Generics.Uniplate.Data
  
import Language.Cobble.Prelude
import Language.Cobble.Types.AST
import Language.Cobble.Types.TH
  
-- Needed for @ModSig@ instances
import Language.Cobble.Types.AST.Codegen
  
deriving instance Show     (Module 'QualifyNames)
deriving instance Eq       (Module 'QualifyNames)
deriving instance Generic  (Module 'QualifyNames)
deriving instance Data     (Module 'QualifyNames)
deriving instance Typeable (Module 'QualifyNames)
  
deriving instance Show     (Statement 'QualifyNames)
deriving instance Eq       (Statement 'QualifyNames)
deriving instance Generic  (Statement 'QualifyNames)
deriving instance Data     (Statement 'QualifyNames)
deriving instance Typeable (Statement 'QualifyNames)
  
deriving instance Show     (Expr 'QualifyNames)
deriving instance Eq       (Expr 'QualifyNames)
deriving instance Generic  (Expr 'QualifyNames)
deriving instance Data     (Expr 'QualifyNames)
deriving instance Typeable (Expr 'QualifyNames)

deriving instance Show     (Type 'QualifyNames)
deriving instance Eq       (Type 'QualifyNames) 
deriving instance Generic  (Type 'QualifyNames) 
deriving instance Data     (Type 'QualifyNames) 
deriving instance Typeable (Type 'QualifyNames) 
  
type instance XModule 'QualifyNames = Map (Name 'Codegen) ModSig
  
type instance XDef       'QualifyNames = ()
type instance XImport    'QualifyNames = ()
type instance XDefStruct 'QualifyNames = ()
type instance XStatement 'QualifyNames = Void


type instance XFCall   'QualifyNames = ()
type instance XIntLit  'QualifyNames = ()
type instance XBoolLit 'QualifyNames = ()
type instance XIf      'QualifyNames = ()
type instance XVar     'QualifyNames = ()
type instance XLet     'QualifyNames = ()
type instance XExpr    'QualifyNames = Void

type instance Name 'QualifyNames = Text

type instance XKind 'QualifyNames = ()

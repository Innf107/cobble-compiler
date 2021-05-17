{-#OPTIONS_GHC -Wno-orphans#-}
{-# LANGUAGE TemplateHaskell#-}
module Language.Cobble.Types.AST.SolveModules where
  
import Data.Data
import Data.Generics.Uniplate.Data
  
import Language.Cobble.Prelude
import Language.Cobble.Types.AST
import Language.Cobble.Types.TH
  
deriving instance Show     (Module 'SolveModules)
deriving instance Eq       (Module 'SolveModules)
deriving instance Generic  (Module 'SolveModules)
deriving instance Data     (Module 'SolveModules)
deriving instance Typeable (Module 'SolveModules)

deriving instance Show     (Statement 'SolveModules)
deriving instance Eq       (Statement 'SolveModules)
deriving instance Generic  (Statement 'SolveModules)
deriving instance Data     (Statement 'SolveModules)
deriving instance Typeable (Statement 'SolveModules)
  
deriving instance Show     (Expr 'SolveModules)
deriving instance Eq       (Expr 'SolveModules)
deriving instance Generic  (Expr 'SolveModules)
deriving instance Data     (Expr 'SolveModules)
deriving instance Typeable (Expr 'SolveModules)

deriving instance Show     (Type 'SolveModules)
deriving instance Eq       (Type 'SolveModules) 
deriving instance Generic  (Type 'SolveModules) 
deriving instance Data     (Type 'SolveModules) 
deriving instance Typeable (Type 'SolveModules) 
  
type instance XModule 'SolveModules = ()
  
type instance XDef           'SolveModules = ()
type instance XImport        'SolveModules = ()
type instance XDefStruct     'SolveModules = ()
type instance XStatement     'SolveModules = Void


type instance XFCall   'SolveModules = ()
type instance XIntLit  'SolveModules = ()
type instance XBoolLit 'SolveModules = ()
type instance XIf      'SolveModules = ()
type instance XVar     'SolveModules = ()
type instance XLet     'SolveModules = ()
type instance XExpr    'SolveModules = Void

type instance Name 'SolveModules = Text

type instance XKind 'SolveModules = ()

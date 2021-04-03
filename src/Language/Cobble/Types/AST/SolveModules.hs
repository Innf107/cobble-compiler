{-#OPTIONS_GHC -Wno-orphans#-}
{-# LANGUAGE TemplateHaskell#-}
module Language.Cobble.Types.AST.SolveModules where
  
import Language.Cobble.Prelude
import Language.Cobble.Types.AST
import Language.Cobble.Types.TH
  
deriving instance Show (Statement 'SolveModules)
deriving instance Eq   (Statement 'SolveModules)
  
deriving instance Show (Expr 'SolveModules)
deriving instance Eq   (Expr 'SolveModules)

deriving instance Show (Type 'SolveModules)
deriving instance Eq   (Type 'SolveModules) 
  
type instance XModule 'SolveModules = ()
  
type instance XCallFun       'SolveModules = ()
type instance XDefFun        'SolveModules = ()
type instance XImport        'SolveModules = ()
type instance XDefVoid       'SolveModules = ()
type instance XDecl          'SolveModules = ()
type instance XAssign        'SolveModules = ()
type instance XWhile         'SolveModules = ()
type instance XDefStruct     'SolveModules = ()
type instance XSetScoreboard 'SolveModules = ()
type instance XStatement     'SolveModules = ()


type instance XFCall   'SolveModules = ()
type instance XIntLit  'SolveModules = ()
type instance XBoolLit 'SolveModules = ()
type instance XVar     'SolveModules = ()
type instance XExpr    'SolveModules = ()

type instance Name 'SolveModules = Text

type instance XKind 'SolveModules = ()

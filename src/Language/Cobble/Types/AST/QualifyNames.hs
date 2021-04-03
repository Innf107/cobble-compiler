{-#OPTIONS_GHC -Wno-orphans#-}
{-# LANGUAGE TemplateHaskell#-}
module Language.Cobble.Types.AST.QualifyNames where
  
import Language.Cobble.Prelude
import Language.Cobble.Types.AST
import Language.Cobble.Types.TH
  
deriving instance Show (Statement 'QualifyNames)
deriving instance Eq (Statement 'QualifyNames)
  
deriving instance Show (Expr 'QualifyNames)
deriving instance Eq (Expr 'QualifyNames)

deriving instance Show (Type 'QualifyNames)
deriving instance Eq (Type 'QualifyNames) 
  
type instance XModule 'QualifyNames = ()
  
type instance XCallFun 'QualifyNames = ()
type instance XDefFun 'QualifyNames = ()
type instance XImport 'QualifyNames = ()
type instance XDefVoid 'QualifyNames = ()
type instance XDecl 'QualifyNames = ()
type instance XAssign 'QualifyNames = ()
type instance XWhile 'QualifyNames = ()
type instance XDefStruct 'QualifyNames = ()
type instance XSetScoreboard 'QualifyNames = ()
type instance XStatement 'QualifyNames = ()


type instance XFCall 'QualifyNames = ()
type instance XIntLit 'QualifyNames = ()
type instance XBoolLit 'QualifyNames = ()
type instance XVar 'QualifyNames = ()
type instance XExpr 'QualifyNames = ()

type instance Name 'QualifyNames = Text

type instance XKind 'QualifyNames = ()

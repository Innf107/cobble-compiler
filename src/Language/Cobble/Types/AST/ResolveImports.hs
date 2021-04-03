{-#OPTIONS_GHC -Wno-orphans#-}
{-# LANGUAGE TemplateHaskell#-}
module Language.Cobble.Types.AST.ResolveImports where
  
import Language.Cobble.Prelude
import Language.Cobble.Types.AST
import Language.Cobble.Types.TH
  
deriving instance Show (Statement 'ResolveImports)
deriving instance Eq   (Statement 'ResolveImports)
  
deriving instance Show (Expr 'ResolveImports)
deriving instance Eq   (Expr 'ResolveImports)

deriving instance Show (Type 'ResolveImports)
deriving instance Eq   (Type 'ResolveImports) 
  
type instance XModule 'ResolveImports = ()
  
type instance XCallFun       'ResolveImports = ()
type instance XDefFun        'ResolveImports = ()
type instance XImport        'ResolveImports = ()
type instance XDefVoid       'ResolveImports = ()
type instance XDecl          'ResolveImports = ()
type instance XAssign        'ResolveImports = ()
type instance XWhile         'ResolveImports = ()
type instance XDefStruct     'ResolveImports = ()
type instance XSetScoreboard 'ResolveImports = ()
type instance XStatement     'ResolveImports = ()


type instance XFCall   'ResolveImports = ()
type instance XIntLit  'ResolveImports = ()
type instance XBoolLit 'ResolveImports = ()
type instance XVar     'ResolveImports = ()
type instance XExpr    'ResolveImports = ()

type instance Name 'ResolveImports = Text

type instance XKind 'ResolveImports = ()

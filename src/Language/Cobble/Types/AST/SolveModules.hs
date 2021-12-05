{-#OPTIONS_GHC -Wno-orphans#-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances#-}
module Language.Cobble.Types.AST.SolveModules where
    
import Language.Cobble.Prelude
import Language.Cobble.Types.AST
    
type instance XModule 'SolveModules = IgnoreExt SolveModules
  
type instance XDef              SolveModules = Ext SolveModules (Maybe Fixity)
type instance XDecl             SolveModules = IgnoreExt SolveModules
type instance XParam            SolveModules = Ext SolveModules [(Name 'SolveModules)]
type instance XImport           SolveModules = IgnoreExt SolveModules
type instance XDefStruct        SolveModules = IgnoreExt SolveModules
type instance XDefVariant       SolveModules = IgnoreExt SolveModules
type instance XDefVariantClause SolveModules = IgnoreExt SolveModules
type instance XDefClass         SolveModules = IgnoreExt SolveModules
type instance XDefInstance      SolveModules = IgnoreExt SolveModules
type instance XStatement        SolveModules = ExtVoid SolveModules


type instance XFCall            'SolveModules = IgnoreExt SolveModules
type instance XIntLit           'SolveModules = IgnoreExt SolveModules
type instance XIf               'SolveModules = IgnoreExt SolveModules
type instance XLet              'SolveModules = IgnoreExt SolveModules
type instance XVar              'SolveModules = IgnoreExt SolveModules
type instance XVariantConstr    'SolveModules = IgnoreExt SolveModules
type instance XStructConstruct  'SolveModules = IgnoreExt SolveModules
type instance XStructAccess     'SolveModules = IgnoreExt 'SolveModules
type instance XExpr             'SolveModules = OperatorGroup SolveModules NoFixity

type instance Name 'SolveModules = Text

type instance XKind 'SolveModules = ()

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.QualifyNames where
    
import Language.Cobble.Prelude
import Language.Cobble.Types.AST
    
type instance XModule 'QualifyNames = Ext QualifyNames (Map (Name 'Codegen) ModSig)
  
type instance XDef          'QualifyNames = Ext QualifyNames (Maybe Fixity)
type instance XDecl         'QualifyNames = IgnoreExt QualifyNames
type instance XParam        'QualifyNames = Ext QualifyNames [Name QualifyNames]
type instance XImport       'QualifyNames = IgnoreExt QualifyNames
type instance XDefStruct    'QualifyNames = IgnoreExt QualifyNames
type instance XDefVariant   'QualifyNames = IgnoreExt QualifyNames
type instance XStatement    'QualifyNames = ExtVoid QualifyNames


type instance XFCall            'QualifyNames = IgnoreExt QualifyNames
type instance XIntLit           'QualifyNames = IgnoreExt QualifyNames
type instance XIf               'QualifyNames = IgnoreExt QualifyNames
type instance XLet              'QualifyNames = IgnoreExt QualifyNames
type instance XVar              'QualifyNames = IgnoreExt QualifyNames
type instance XVariantConstr    'QualifyNames = IgnoreExt QualifyNames
type instance XStructConstruct  'QualifyNames = IgnoreExt QualifyNames
type instance XStructAccess     'QualifyNames = IgnoreExt QualifyNames
type instance XExpr             'QualifyNames = OperatorGroup QualifyNames NoFixity

type instance Name 'QualifyNames = Text

type instance XKind 'QualifyNames = ()

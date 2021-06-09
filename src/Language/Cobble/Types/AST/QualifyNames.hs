{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.QualifyNames where
  
import Data.Data
import Data.Generics.Uniplate.Data
  
import Language.Cobble.Prelude
import Language.Cobble.Types.AST
    
type instance XModule 'QualifyNames = Map (Name 'Codegen) ModSig
  
type instance XDef       'QualifyNames = ()
type instance XDecl      'QualifyNames = ()
type instance XParam     'QualifyNames = [Name 'QualifyNames]
type instance XImport    'QualifyNames = ()
type instance XDefStruct 'QualifyNames = ()
type instance XStatement 'QualifyNames = Void


type instance XFCall            'QualifyNames = ()
type instance XIntLit           'QualifyNames = ()
type instance XIf               'QualifyNames = ()
type instance XLet              'QualifyNames = ()
type instance XVar              'QualifyNames = ()
type instance XStructConstruct  'QualifyNames = IgnoreExt 'QualifyNames
type instance XExpr             'QualifyNames = Void

type instance Name 'QualifyNames = Text

type instance XKind 'QualifyNames = ()

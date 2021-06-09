{-#OPTIONS_GHC -Wno-orphans#-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances#-}
module Language.Cobble.Types.AST.SolveModules where
  
import Data.Data
import Data.Generics.Uniplate.Data
  
import Language.Cobble.Prelude
import Language.Cobble.Types.AST
    
type instance XModule 'SolveModules = ()
  
type instance XDef           'SolveModules = ()
type instance XDecl          'SolveModules = ()
type instance XParam         'SolveModules = [(Name 'SolveModules)]
type instance XImport        'SolveModules = ()
type instance XDefStruct     'SolveModules = ()
type instance XStatement     'SolveModules = Void


type instance XFCall            'SolveModules = ()
type instance XIntLit           'SolveModules = ()
type instance XIf               'SolveModules = ()
type instance XLet              'SolveModules = ()
type instance XVar              'SolveModules = ()
type instance XStructConstruct  'SolveModules = IgnoreExt SolveModules
type instance XExpr             'SolveModules = Void

type instance Name 'SolveModules = Text

type instance XKind 'SolveModules = ()

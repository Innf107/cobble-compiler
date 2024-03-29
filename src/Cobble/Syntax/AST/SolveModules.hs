{-#OPTIONS_GHC -Wno-orphans#-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances#-}
module Cobble.Syntax.AST.SolveModules where
    
import Cobble.Prelude
import Cobble.Syntax.AST
    
type instance XModule 'SolveModules = ()
  
type instance XDef              SolveModules = Maybe Fixity
type instance XDecl             SolveModules = ()
type instance XParam            SolveModules = Seq (Name 'SolveModules)
type instance XImport           SolveModules = ()
type instance XDefVariant       SolveModules = ()
type instance XDefVariantClause SolveModules = ()
type instance XDefEffect        SolveModules = ()
type instance XDefClass         SolveModules = ()
type instance XDefInstance      SolveModules = ()
type instance XStatement        SolveModules = Void


type instance XFCall            'SolveModules = ()
type instance XIntLit           'SolveModules = ()
type instance XIf               'SolveModules = ()
type instance XLet              'SolveModules = ()
type instance XVar              'SolveModules = ()
type instance XAscription       'SolveModules = ()
type instance XVariantConstr    'SolveModules = ()
type instance XCase             'SolveModules = ()

type instance XLambda           SolveModules = ()
type instance XHandle           SolveModules = ()
type instance XResume           SolveModules = ()

type instance XExpr             'SolveModules = Either (OperatorGroup SolveModules NoFixity) UnitLit

type instance XCaseBranch SolveModules = ()

type instance XEffHandler SolveModules = ()

type instance XIntP         SolveModules = ()
type instance XVarP         SolveModules = ()
type instance XConstrP      SolveModules = ()
type instance XWildcardP    SolveModules = ()
type instance XOrP          SolveModules = ()
type instance XPattern      SolveModules = Void

type instance Name 'SolveModules = Text

type instance XKind 'SolveModules = ()

type instance XType SolveModules = UType
type instance XTVar SolveModules = (Text, Maybe Kind)

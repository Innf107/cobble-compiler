{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.QualifyNames where
    
import Language.Cobble.Prelude
import Language.Cobble.Types.AST
    
type instance XModule 'QualifyNames = Map (Name 'Codegen) ModSig

type instance XDef              QualifyNames = Maybe Fixity
type instance XDecl             QualifyNames = ()
type instance XParam            QualifyNames = [Name QualifyNames]
type instance XImport           QualifyNames = ()
type instance XDefStruct        QualifyNames = ()
type instance XDefVariant       QualifyNames = ()
type instance XDefVariantClause QualifyNames = ()
type instance XDefClass         QualifyNames = ()
type instance XDefInstance      QualifyNames = ()
type instance XStatement        QualifyNames = Void


type instance XFCall            'QualifyNames = ()
type instance XIntLit           'QualifyNames = ()
type instance XIf               'QualifyNames = ()
type instance XLet              'QualifyNames = ()
type instance XVar              'QualifyNames = ()
type instance XAscription       'QualifyNames = ()
type instance XVariantConstr    'QualifyNames = ()
type instance XCase             'QualifyNames = ()
type instance XStructConstruct  'QualifyNames = ()
type instance XStructAccess     'QualifyNames = ()

type instance XLambda           QualifyNames = ()

type instance XExpr             'QualifyNames = OperatorGroup QualifyNames NoFixity

type instance XCaseBranch QualifyNames = ()

type instance XIntP     QualifyNames = ()
type instance XVarP     QualifyNames = ()
type instance XConstrP  QualifyNames = ()
type instance XPattern  QualifyNames = Void

type instance Name 'QualifyNames = Text

type instance XKind 'QualifyNames = ()

type instance XType QualifyNames = UType
type instance XTVar QualifyNames = Text -- = UnqualifiedName


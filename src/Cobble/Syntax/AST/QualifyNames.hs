{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Cobble.Syntax.AST.QualifyNames where
    
import Cobble.Prelude
import Cobble.Syntax.AST
    
type instance XModule QualifyNames = Map Text ModSig

type instance XDef              QualifyNames = Maybe Fixity
type instance XDecl             QualifyNames = ()
type instance XParam            QualifyNames = (Seq (Name QualifyNames))
type instance XImport           QualifyNames = Void
type instance XDefVariant       QualifyNames = ()
type instance XDefVariantClause QualifyNames = ()
type instance XDefEffect        QualifyNames = ()
type instance XDefClass         QualifyNames = ()
type instance XDefInstance      QualifyNames = ()
type instance XStatement        QualifyNames = Void


type instance XFCall            QualifyNames = ()
type instance XIntLit           QualifyNames = ()
type instance XIf               QualifyNames = ()
type instance XLet              QualifyNames = ()
type instance XVar              QualifyNames = ()
type instance XAscription       QualifyNames = ()
type instance XVariantConstr    QualifyNames = ()
type instance XCase             QualifyNames = ()

type instance XLambda           QualifyNames = ()

type instance XHandle           QualifyNames = ()
type instance XResume           QualifyNames = ()

type instance XExpr             QualifyNames = Either (OperatorGroup QualifyNames NoFixity) UnitLit

type instance XCaseBranch QualifyNames = ()

type instance XEffHandler QualifyNames = ()

type instance XIntP         QualifyNames = ()
type instance XVarP         QualifyNames = ()
type instance XConstrP      QualifyNames = ()
type instance XWildcardP    QualifyNames = ()
type instance XOrP          QualifyNames = ()
type instance XPattern      QualifyNames = Void

type instance Name QualifyNames = Text

type instance XKind QualifyNames = ()

type instance XType QualifyNames = UType
type instance XTVar QualifyNames = (Text, Maybe Kind)


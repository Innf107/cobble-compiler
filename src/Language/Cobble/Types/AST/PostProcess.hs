{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.PostProcess where
  
import Data.Data hiding (Fixity)
import Data.Generics.Uniplate.Data
  
import Language.Cobble.Prelude

import Language.Cobble.Types.AST
import Language.Cobble.Types.Instances
import Language.Cobble.Types.QualifiedName

deriving instance Show TWanted
deriving instance Eq TWanted
deriving instance Generic TWanted
deriving instance Data TWanted


deriving instance Show TGiven
deriving instance Eq TGiven
deriving instance Generic TGiven
deriving instance Data TGiven

type instance XModule PostProcess = Map (Name PostProcess) ModSig

type instance XDecl             PostProcess = (Type PostProcess, [TGiven])
type instance XParam            PostProcess = [(Name PostProcess, Type PostProcess)]
type instance XDef              PostProcess = Maybe Fixity
type instance XImport           PostProcess = ()
type instance XDefStruct        PostProcess = Kind
type instance XDefVariant       PostProcess = Kind
type instance XDefVariantClause PostProcess = (Type PostProcess, Int, Int)
type instance XDefClass         PostProcess = Kind
-- XDefInstance uses a list of pairs instead of a Map, because SemAnalysis shuffles declarations around
-- to have the same order as class declaration.
type instance XDefInstance      PostProcess = ([(QualifiedName, Type Codegen)], [TVar Codegen])
type instance XStatement        PostProcess = Void

type instance XFCall            PostProcess = (Type PostProcess)
type instance XIntLit           PostProcess = ()
type instance XIf               PostProcess = ()
type instance XLet              PostProcess = ()
type instance XVar              PostProcess = (Type PostProcess, [TWanted])
type instance XAscription       PostProcess = Void
type instance XVariantConstr    PostProcess = (Type PostProcess, Int, Int)
--                                                                ^    ^
--                                                                |    constructor index
--                                                                expected number of args
type instance XCase             PostProcess = Type PostProcess

type instance XStructConstruct  PostProcess = (StructDef PostProcess, Type PostProcess)
type instance XStructAccess     PostProcess = (Map QualifiedName (StructDef PostProcess), Type PostProcess, Type PostProcess)

type instance XLambda           PostProcess = Type PostProcess

type instance XExpr             PostProcess = Void

type instance XCaseBranch PostProcess = ()

type instance XIntP     PostProcess = ()
type instance XVarP     PostProcess = Type PostProcess
type instance XConstrP  PostProcess = (Type PostProcess, Int)
type instance XPattern  PostProcess = Void


type instance Name PostProcess = QualifiedName

type instance XKind PostProcess = Kind

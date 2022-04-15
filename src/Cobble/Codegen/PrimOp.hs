module Cobble.Codegen.PrimOp where

import Cobble.Prelude hiding (EQ, Debug)
import Cobble.Util
import Cobble.Types

import Data.Map qualified as M
import qualified GHC.Exts

data PrimOp = True_
            | False_
            | Add
            | Sub
            | Mul
            | IntDiv
            | Mod
            | LE
            | EQ
            | Debug
            deriving (Show, Eq, Generic, Data)
instance Binary PrimOp

data PrimOpInfo = PrimOpInfo {
        primOp :: PrimOp
    ,   primOpType :: Type
    }


primOps :: Map QualifiedName PrimOpInfo 
primOps = M.mapKeys (\k -> internalQName k) $ fromList [
        ("add#",    PrimOpInfo Add      let f1 = internalEffFun "μ1" in let f2 = internalEffFun "μ2" in (intT `f1` (intT `f2` intT)))
    ,   ("sub#",    PrimOpInfo Sub      let f1 = internalEffFun "μ1" in let f2 = internalEffFun "μ2" in (intT `f1` (intT `f2` intT)))
    ,   ("mul#",    PrimOpInfo Mul      let f1 = internalEffFun "μ1" in let f2 = internalEffFun "μ2" in (intT `f1` (intT `f2` intT)))
    ,   ("intdiv#", PrimOpInfo IntDiv   let f1 = internalEffFun "μ1" in let f2 = internalEffFun "μ2" in (intT `f1` (intT `f2` intT)))
    ,   ("mod#",    PrimOpInfo Mod      let f1 = internalEffFun "μ1" in let f2 = internalEffFun "μ2" in (intT `f1` (intT `f2` intT)))
    ,   ("le#",     PrimOpInfo LE       let f1 = internalEffFun "μ1" in let f2 = internalEffFun "μ2" in (intT `f1` (intT `f2` boolT)))
    ,   ("eq#",     PrimOpInfo EQ       let f1 = internalEffFun "μ1" in let f2 = internalEffFun "μ2" in (intT `f1` (intT `f2` boolT)))
    ,   ("debug#",  PrimOpInfo Debug    let debugArg = MkTVar (internalQName "debugArg") KStar in let f = internalEffFun "μ" in (TForall [debugArg] $ TVar debugArg `f` unitT))
    ] where
        internalEff x = MkTVar (internalQName x) (KRow KEffect)
        internalEffFun effName x y = TFun x (TVar (internalEff effName)) y 



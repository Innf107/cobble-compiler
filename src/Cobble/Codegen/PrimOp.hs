module Cobble.Codegen.PrimOp where

import Cobble.Prelude hiding (EQ)
import Cobble.Util
import Cobble.Syntax

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
        ("add#",    PrimOpInfo Add      let (e1, f1) = internalEffFun "μ1" in let (e2, f2) = internalEffFun "μ2" in TForall [e1, e2] (intT `f1` (intT `f2` intT)))
    ,   ("sub#",    PrimOpInfo Sub      let (e1, f1) = internalEffFun "μ1" in let (e2, f2) = internalEffFun "μ2" in TForall [e1, e2] (intT `f1` (intT `f2` intT)))
    ,   ("mul#",    PrimOpInfo Mul      let (e1, f1) = internalEffFun "μ1" in let (e2, f2) = internalEffFun "μ2" in TForall [e1, e2] (intT `f1` (intT `f2` intT)))
    ,   ("intdiv#", PrimOpInfo IntDiv   let (e1, f1) = internalEffFun "μ1" in let (e2, f2) = internalEffFun "μ2" in TForall [e1, e2] (intT `f1` (intT `f2` intT)))
    ,   ("mod#",    PrimOpInfo Mod      let (e1, f1) = internalEffFun "μ1" in let (e2, f2) = internalEffFun "μ2" in TForall [e1, e2] (intT `f1` (intT `f2` intT)))
    ,   ("le#",     PrimOpInfo LE       let (e1, f1) = internalEffFun "μ1" in let (e2, f2) = internalEffFun "μ2" in TForall [e1, e2] (intT `f1` (intT `f2` boolT)))
    ,   ("eq#",     PrimOpInfo EQ       let (e1, f1) = internalEffFun "μ1" in let (e2, f2) = internalEffFun "μ2" in TForall [e1, e2] (intT `f1` (intT `f2` boolT)))
    ,   ("debug#",  PrimOpInfo Debug    let debugArg = MkTVar (internalQName "debugArg") KStar in let (e, f) = internalEffFun "μ" in (TForall [debugArg, e] $ TVar debugArg `f` unitT))
    ] where
        internalEff x = MkTVar (internalQName x) (KRow KEffect)
        internalEffFun effName = let effVar = internalEff effName in (effVar, \x y -> TFun x (TVar effVar) y)



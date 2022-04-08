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

data PrimOpInfo = PrimOpInfo {
        primOp :: PrimOp
    ,   primOpType :: Type
    }


primOps :: Map QualifiedName PrimOpInfo 
primOps = M.mapKeys (\k -> internalQName k) $ fromList [
        ("true#",   PrimOpInfo True_    (unitT :-> boolT))
    ,   ("false#",  PrimOpInfo False_   (unitT :-> boolT))          
    ,   ("add#",    PrimOpInfo Add      (intT :-> intT :-> intT)) 
    ,   ("sub#",    PrimOpInfo Sub      (intT :-> intT :-> intT)) 
    ,   ("mul#",    PrimOpInfo Mul      (intT :-> intT :-> intT)) 
    ,   ("intdiv#", PrimOpInfo IntDiv   (intT :-> intT :-> intT)) 
    ,   ("mod#",    PrimOpInfo Mod      (intT :-> intT :-> intT)) 
    ,   ("le#",     PrimOpInfo LE       (intT :-> intT :-> boolT))
    ,   ("eq#",     PrimOpInfo EQ       (intT :-> intT :-> boolT))
    ,   ("debug#",  PrimOpInfo Debug    let debugArg = MkTVar (internalQName "debugArg") KStar in (TForall [debugArg] $ TVar debugArg :-> unitT))
    ]



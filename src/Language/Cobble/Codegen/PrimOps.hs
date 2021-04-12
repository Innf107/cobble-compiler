module Language.Cobble.Codegen.PrimOps (primOps) where

import Language.Cobble.Prelude
import Language.Cobble.Util
import Language.Cobble.Types
import Language.Cobble.Codegen.Types
import Language.Cobble.MCAsm.Types

import Data.Map qualified as M

primOps :: (PrimOpC r) => Map QualifiedName (PrimOp r)
primOps = M.mapKeys ("prims" .:) $ fromList [
        ("_add", ([("x", intT), ("y", intT)], Just intT, _add))
    ]

_add :: (PrimOpC r) => PrimOpF r
_add PrimOpEnv{..} args = do
    aregs <- traverse compileExprToReg args
    let [r1, r2] = aregs
    res <- NumReg <$> newReg TempReg
    tell [MoveReg res r1, AddReg res r2]
    pure res
    

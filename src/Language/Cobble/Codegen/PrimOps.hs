module Language.Cobble.Codegen.PrimOps (primOps) where

import Language.Cobble.Prelude
import Language.Cobble.Util
import Language.Cobble.Types
import Language.Cobble.Codegen.Types
import Language.Cobble.MCAsm.Types
import Language.Cobble.MCAsm.McFunction

import Data.Map qualified as M

primOps :: (PrimOpC r) => Map QualifiedName (PrimOp r)
primOps = M.mapKeys ("prims" .:) $ fromList [
        ("_add", ([("x", intT), ("y", intT)], Just intT, _add))
    ,   ("_le", ([("x", intT), ("y", intT)], Just boolT, _le))
    ]

_add :: (PrimOpC r) => PrimOpF r
_add PrimOpEnv{..} args = do
    aregs <- traverse compileExprToReg args
    let [r1, r2] = aregs
    res <- newReg TempReg NumReg
    tell [MoveReg res r1, AddReg res r2]
    pure res
    
_le :: (PrimOpC r) => PrimOpF r
_le PrimOpEnv{..} args = do
    aregs <- traverse compileExprToReg args
    let [r1, r2] = aregs
    res <- newReg TempReg NumReg
    tell [MoveNumLit res 0, ExecLE r1 r2 [McFunction $ "scoreboard players set " <> renderReg res <> " REGS 1"]] --TODO
    pure res 
    
    
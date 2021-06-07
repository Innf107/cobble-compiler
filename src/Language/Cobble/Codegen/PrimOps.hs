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
--      Literals
        ("_true", (unitT -:> boolT, _true))
    ,   ("_false", (unitT -:> boolT, _false))
--      Arithmetic
    ,   ("_add", (intT -:> intT -:> intT, _add))
    ,   ("_sub", (intT -:> intT -:> intT, _sub))
    ,   ("_mul", (intT -:> intT -:> intT, _mul))
    ,   ("_div", (intT -:> intT -:> intT, _div))
    ,   ("_mod", (intT -:> intT -:> intT, _mod))
--      Comparison
    ,   ("_le", (intT -:> intT -:> boolT, _le))
--      Side Effects
    ,   ("_setTestScoreboardUnsafe", (intT -:> unitT, _setTestScoreboardUnsafe))
    ]

_true :: (PrimOpC r) => PrimOpF r
_true PrimOpEnv{..} _args = pure trueReg

_false :: (PrimOpC r) => PrimOpF r
_false PrimOpEnv{..} _args = pure falseReg

_add :: (PrimOpC r) => PrimOpF r
_add PrimOpEnv{..} args = do
    aregs <- traverse compileExprToReg args
    let [r1, r2] = aregs
    res <- newReg TempReg NumReg
    tell [MoveReg res r1, AddReg res r2]
    pure res

_sub :: (PrimOpC r) => PrimOpF r
_sub PrimOpEnv{..} args = do
    aregs <- traverse compileExprToReg args
    let [r1, r2] = aregs
    res <- newReg TempReg NumReg
    tell [MoveReg res r1, SubReg res r2]
    pure res

_mul :: (PrimOpC r) => PrimOpF r
_mul PrimOpEnv{..} args = do
    aregs <- traverse compileExprToReg args
    let [r1, r2] = aregs
    res <- newReg TempReg NumReg
    tell [MoveReg res r1, MulReg res r2]
    pure res

_div :: (PrimOpC r) => PrimOpF r
_div PrimOpEnv{..} args = do
    aregs <- traverse compileExprToReg args
    let [r1, r2] = aregs
    res <- newReg TempReg NumReg
    tell [MoveReg res r1, DivReg res r2]
    pure res

_mod :: (PrimOpC r) => PrimOpF r
_mod PrimOpEnv{..} args = do
    aregs <- traverse compileExprToReg args
    let [r1, r2] = aregs
    res <- newReg TempReg NumReg
    tell [MoveReg res r1, ModReg res r2]
    pure res

_le :: (PrimOpC r) => PrimOpF r
_le PrimOpEnv{..} args = do
    aregs <- traverse compileExprToReg args
    let [r1, r2] = aregs
    res <- newReg TempReg NumReg
    tell [MoveNumLit res 0, ExecLE r1 r2 [McFunction $ "scoreboard players set " <> renderReg res <> " REGS 1"]] --TODO
    pure res 

_setTestScoreboardUnsafe :: (PrimOpC r) => PrimOpF r
_setTestScoreboardUnsafe PrimOpEnv{..} args = do
    aregs <- traverse compileExprToReg args
    let [r1] = aregs
    tell [SetScoreboard (Objective "test") "test" r1]
    pure unitReg
    
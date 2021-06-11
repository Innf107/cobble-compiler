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

_true :: PrimOpF r
_true PrimOpEnv{..} _args = pure trueReg

_false :: PrimOpF r
_false PrimOpEnv{..} _args = pure falseReg

_add :: (Members '[Writer [Instruction], Error Panic] r) => PrimOpF r
_add = binaryOp NumReg \_ x y res -> tell [MoveReg res x, AddReg res y]

_sub :: (Members '[Writer [Instruction], Error Panic] r) => PrimOpF r
_sub = binaryOp NumReg \_ x y res -> tell [MoveReg res x, SubReg res y]

_mul :: (Members '[Writer [Instruction], Error Panic] r) => PrimOpF r
_mul = binaryOp NumReg \_ x y res -> tell [MoveReg res x, MulReg res y]

_div :: (Members '[Writer [Instruction], Error Panic] r) => PrimOpF r
_div = binaryOp NumReg \_ x y res -> tell [MoveReg res x, DivReg res y]

_mod :: (Members '[Writer [Instruction], Error Panic] r) => PrimOpF r
_mod = binaryOp NumReg \_ x y res -> tell [MoveReg res x, ModReg res y]

_le :: (Members '[Writer [Instruction], Error Panic] r) => PrimOpF r
_le = binaryOp NumReg \_ x y res -> tell [MoveNumLit res 0, ExecLE x y [McFunction $ "scoreboard players set " <> renderReg res <> " REGS 1"]] --TODO!

_setTestScoreboardUnsafe :: (Members '[Writer [Instruction], Error Panic] r) => PrimOpF r
_setTestScoreboardUnsafe = unaryOp' \PrimOpEnv{..} x -> tell [SetScoreboard (Objective "test") "test" x] $> unitReg


-- Helper functions
unaryOp :: (Member (Error Panic) r) => (RegId -> Register) -> (PrimOpEnv r -> Register -> Register -> Sem r ()) -> PrimOpF r
unaryOp resRep f = unaryOp' \e@PrimOpEnv{..} x -> do
    resReg <- newReg TempReg resRep
    f e x resReg
    pure resReg

unaryOp' :: (Member (Error Panic) r) => (PrimOpEnv r -> Register -> Sem r Register) -> PrimOpF r
unaryOp' f e@PrimOpEnv{..} args = do
    aregs <- traverse compileExprToReg args
    case aregs of
        [r1] -> f e r1
        _ -> panic $ "unary primop applied the wrong amount of arguments (" <> (show $ length aregs) <> "). Is the type correct?"

binaryOp :: (Member (Error Panic) r) => (RegId -> Register) -> (PrimOpEnv r -> Register -> Register -> Register -> Sem r ()) -> PrimOpF r
binaryOp resRep f e@PrimOpEnv{..} args = do
    aregs <- traverse compileExprToReg args
    resReg <- newReg TempReg resRep
    case aregs of
        [r1, r2] -> f e r1 r2 resReg >> pure resReg
        _ -> panic $ "binary primop applied the wrong amount of arguments (" <> (show $ length aregs) <> "). Is the type correct?"


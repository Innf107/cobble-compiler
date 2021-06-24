{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Codegen.CPSToAsm where

import Language.Cobble.Prelude
import Language.Cobble.MCAsm.Types as A
import Language.Cobble.CPS.Types as C

data CPSToAsmState = CPSToAsmState {
    _varRegs :: Map CPSVar Register
,   _lastReg :: Int
}
makeLenses ''CPSToAsmState

varReg :: CPSVar -> Lens' CPSToAsmState (Maybe Register)
varReg n = varRegs . at n

freshReg :: (Member (State CPSToAsmState) r) => Sem r Register
freshReg = state (\s -> (Reg (view lastReg s), over lastReg (+1) s))

compile :: (Members [State CPSToAsmState, Writer [Instruction]] r) => [CPSDef] -> Sem r ()
compile = traverse_ compileDef

compileDef :: (Members [State CPSToAsmState, Writer [Instruction]] r) => CPSDef -> Sem r ()
compileDef (CPSDef name body) = void $ censor (pure . Section name) $ compileExpr body

compileExpr :: (Members [State CPSToAsmState, Writer [Instruction]] r, HasCallStack) => CPSExpr -> Sem r Register
compileExpr = undefined {-\case
    Var v    -> fromMaybe (error $ "compileExpr: variable not in scope: " <> show v) . view (varReg v) <$> get
    -- TODO: Move Int lits to program start
    IntLit i -> do
        r <- freshReg
        tell [MoveNumLit r i]
        pure r
    Lambda v e -> do
        vreg <- freshReg
        modify (set (varReg v) (Just vreg))
        -}

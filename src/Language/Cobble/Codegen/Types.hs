module Language.Cobble.Codegen.Types where

import Language.Cobble.Prelude
import Language.Cobble.Types
import Language.Cobble.MCAsm.Types hiding (Name)

type CompileC r = Members '[State CompileState, Error Panic, Error McAsmError, Output Log] r

data CompileState = CompileState {
        _frames :: NonEmpty Frame
      , _functions :: Map (Name 'Codegen) Function
    } deriving (Show, Eq)

data Frame = Frame {
        _varRegs :: Map (Name 'Codegen) Register
    ,   _lastReg :: Int
    ,   _regs :: [Register]
    } deriving (Show, Eq)

data Function = Function {
      _params :: [(Name 'Codegen, Type 'Codegen)]
    , _returnType :: Type 'Codegen
    } deriving (Show, Eq)

type PrimOpC r = (CompileC r, Member (Writer [Instruction]) r)

type PrimOp r = (Type 'Codegen, PrimOpF r)

type PrimOpF r = PrimOpEnv r -> [Expr 'Codegen] -> Sem r Register

data PrimOpEnv r = PrimOpEnv {
        compileExprToReg :: Expr 'Codegen -> Sem r Register
    ,   newReg :: (Int -> RegId) -> (RegId -> Register) -> Sem r Register
    ,   unitReg :: Register
    ,   trueReg :: Register
    ,   falseReg :: Register
    }
data Rep = RepNum | RepEntity | RepArray deriving (Show, Eq)

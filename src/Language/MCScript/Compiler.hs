{-#LANGUAGE NoImplicitPrelude, ConstraintKinds, DataKinds, LambdaCase, TemplateHaskell#-}
module Language.MCScript.Compiler where

import Language.MCScript.Prelude

import Language.MCScript.Types as S

import Language.MCScript.MCAsm.Types as A

type CompileC r = Members '[State CompileState, Reader ()] r

data CompileState = CompileState {
        _scopes :: [Scope]
      , _lastReg :: Int
    } deriving (Show, Eq)

data Scope = Scope {
        _varRegs :: Map Text Int
    } deriving (Show, Eq)


makeLenses 'Scope
makeLenses 'CompileState

compile :: (CompileC r) => S.Module 'Typed -> Sem r A.Module
compile (S.Module modname stmnts) = A.Module modname . concat <$> traverse compileStatement stmnts

newReg :: (CompileC r) => Sem r Int
newReg = modify (& lastReg +~ 1) >> get <&> (^. lastReg)

-- Make Sure 'scopes' is never empty!
compileStatement :: (CompileC r) => S.Statement 'Typed -> Sem r [Instruction]
compileStatement = \case
    Decl name t expr -> do
        reg <- newReg
        modify (& scopes . _head . varRegs . at name ?~ reg)
        compileExprToReg reg t (fst expr)
    Assign name (expr, t) -> do
        mreg <- get <&> join . (^? scopes . _head . varRegs . at name)
        case mreg of
            Nothing -> undefined -- TODO: VarNotFoundError (Should not even be able to happen?)
            Just reg -> compileExprToReg reg t expr

compileExprToReg :: (CompileC r) => Int -> Type -> Expr 'Typed -> Sem r [Instruction]
compileExprToReg reg type_ expression = case (type_, expression) of
    (IntT, IntLit i) -> pure [MoveNumLit (NumReg reg) i]
    (IntT, Var n) -> get <&> join . (^? scopes . _head . varRegs . at n) <&> \case
        Just vreg -> [MoveNumReg (NumReg reg) (NumReg vreg)]
        Nothing -> undefined --TODO: VarNotFoundError (Should not even be able to happen?)
    -- (FloatT, FloatLit i) -> pure [MoveNumReg (NumReg reg)]
    (IntT, FCall fname args) -> do
        undefined


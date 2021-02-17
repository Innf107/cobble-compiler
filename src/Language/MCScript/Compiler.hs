{-#LANGUAGE NoImplicitPrelude, ConstraintKinds, DataKinds, LambdaCase, TemplateHaskell, OverloadedStrings#-}
module Language.MCScript.Compiler where

import Language.MCScript.Prelude

import Language.MCScript.Types as S

import Language.MCScript.MCAsm.Types as A

type CompileC r = Members '[State CompileState, Reader ()] r

data CompileState = CompileState {
        _frames :: [Frame]
      , _lastReg :: Int
    } deriving (Show, Eq)

data Frame = Frame {
        _varIndices :: Map Text Int
      , _varCount :: Int
    } deriving (Show, Eq)

stackReg :: Register Array
stackReg = CustomReg "STACK"

stackPTRReg :: Register Number
stackPTRReg = CustomReg "STACKPTR"

makeLenses 'Frame
makeLenses 'CompileState

rts :: (CompileC r) => Sem r A.Module
rts = do
    pure $ A.Module "RTS" [
          MoveNumLit stackPTRReg 0 
        , MakeArray stackReg stackPTRReg
        ]

compile :: (CompileC r) => S.Module 'Typed -> Sem r A.Module
compile (S.Module modname stmnts) = A.Module modname . concat <$> traverse compileStatement stmnts

newReg :: (CompileC r) => Sem r Int
newReg = modify (& lastReg +~ 1) >> get <&> (^. lastReg)

-- Make Sure 'scopes' is never empty!
compileStatement :: (CompileC r) => S.Statement 'Typed -> Sem r [Instruction]
compileStatement = \case
    Decl name IntT expr -> do
        exprReg <- newReg
        ixReg <- NumReg <$> newReg
        arrReg <- ArrayReg <$> newReg
        
        varIx <- get <&> fromMaybe undefined . (^? frames . _head . varCount)
        modify (& frames . _head . varCount +~ 1)
        
        modify (& frames . _head . varIndices . at name ?~ varIx)
        
        exprInstrs <- compileExprToReg exprReg IntT (fst expr)
        pure $ exprInstrs ++ [ 
              GetArrayInArray arrReg stackReg stackPTRReg
            , MoveNumLit ixReg varIx 
            , SetNumInArray arrReg ixReg (NumReg exprReg)
            ]
    Assign name (expr, IntT) -> do
        exprReg <- newReg
        varIxReg <- NumReg <$> newReg
        frameReg <- ArrayReg <$> newReg
        mvarIx <- get <&> join . (^? frames . _head . varIndices . at name)
        case mvarIx of   
            Nothing -> undefined -- TODO: VarNotFoundError (Should not even be able to happen?)
            Just varIx -> do
                exprInstrs <-compileExprToReg exprReg IntT expr
                pure $ exprInstrs ++ [
                      GetArrayInArray frameReg stackReg stackPTRReg
                    , MoveNumLit varIxReg varIx
                    , SetNumInArray frameReg varIxReg (NumReg exprReg)
                    ]

compileExprToReg :: (CompileC r) => Int -> Type -> Expr 'Typed -> Sem r [Instruction]
compileExprToReg reg type_ expression = case (type_, expression) of
    (IntT, IntLit i) -> pure [MoveNumLit (NumReg reg) i]
    (IntT, Var n) -> get <&> join . (^? frames . _head . varIndices . at n) >>= \case
        Nothing -> undefined --TODO: VarNotFoundError (Should not even be able to happen?)
        Just vIx -> do
            frameReg <- ArrayReg <$> newReg
            varIxReg <- NumReg <$> newReg
            pure [
                  MoveNumLit varIxReg vIx
                , GetArrayInArray frameReg stackReg stackPTRReg
                , GetNumInArray (NumReg reg) frameReg varIxReg 
                ]
    -- (FloatT, FloatLit i) -> pure [MoveNumReg (NumReg reg)]
    (IntT, FCall fname args) -> do
        undefined


{-# LANGUAGE NoImplicitPrelude, ConstraintKinds, DataKinds, LambdaCase, TemplateHaskell, OverloadedStrings#-}
{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeApplications, ViewPatterns, BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.MCScript.Compiler where

import Language.MCScript.Prelude

import Language.MCScript.Types as S

import Language.MCScript.MCAsm.Types as A

type CompileC r = Members '[State CompileState, Error CompilerError, Error McAsmError] r

data CompilerError = Panic Text deriving (Show, Eq)

panic :: (Member (Error CompilerError) r) => Text -> Sem r a
panic = throw . Panic

panic' :: (Member (Error CompilerError) r) => Text -> [Text] -> Sem r a
panic' t as = panic $ t <> "\n\nContext: " <> unlines (map ("    "<>) as)

panicVarNotFoundTooLate :: (Member (Error CompilerError) r) => Text -> Sem r a
panicVarNotFoundTooLate v = panic $ "Variable " <> v <> " not found. This should have been caught earlier!" 

data CompileState = CompileState {
        _frames :: NonEmpty Frame
      , _lastReg :: Int
      , _functions :: Map Text Function
    } deriving (Show, Eq)

data Frame = Frame {
        _varIndices :: Map Text Int
      , _varCount :: Int
    } deriving (Show, Eq)

data Function = Function {
      _params :: [(Text, Type)]
    , _returnType :: Maybe Type
    } deriving (Show, Eq)

emptyFrame :: Frame
emptyFrame = Frame mempty 0

stackReg :: Register Array
stackReg = CustomReg "STACK"

stackPTRReg :: Register Number
stackPTRReg = CustomReg "STACKPTR"

returnReg :: Register a
returnReg = CustomReg "RETURN"

makeLenses 'Frame
makeLenses 'CompileState
makeLenses 'Function

rts :: (CompileC r) => Sem r A.Module
rts = do
    pure $ A.Module "RTS" [
          MoveNumLit stackPTRReg 0 
        , MakeArray stackReg stackPTRReg
        ]

compile :: (CompileC r) => S.Module 'Typed -> Sem r A.Module
compile (S.Module modname stmnts) = A.Module modname . fst <$> runWriter (traverse compileStatement stmnts)

newReg :: (CompileC r) => Sem r Int
newReg = modify (& lastReg +~ 1) >> get <&> (^. lastReg)

newRegForType :: (CompileC r, FromSomeReg a) => Type -> Sem r (Register a)
newRegForType = \case
    IntT -> castReg . NumReg =<< newReg
    EntityT -> castReg . EntityReg =<< newReg
    BoolT -> castReg . NumReg =<< newReg
    StructT _ -> castReg . ArrayReg =<< newReg
  
compileStatement :: (Member (Writer [Instruction]) r, CompileC r) => S.Statement 'Typed -> Sem r ()
compileStatement = \case
    Decl name t (typedExprContent @'Typed -> expr) -> void $ pushVarToStack name (expr, t)

    Assign name (expr, t) -> do
        varIxReg <- NumReg <$> newReg
        frameReg <- ArrayReg <$> newReg
        mvarIx <- get <&> join . (^? frames . head1 . varIndices . at name)
        case mvarIx of   
            Nothing -> panicVarNotFoundTooLate name
            Just varIx -> do
                exprReg <- compileExprToReg t expr
                tell [
                      GetArrayInArray frameReg stackReg stackPTRReg
                    , MoveNumLit varIxReg varIx
                    , SetNumInArray frameReg varIxReg exprReg
                    ]

pushVarToStack :: (Member (Writer [Instruction]) r, CompileC r) => Text -> TypedExpr 'Typed -> Sem r Int
pushVarToStack name ex = do
    varIx <- get <&> (^. frames . head1 . varCount)
    modify (& frames . head1 . varCount +~ 1)
    modify (& frames . head1 . varIndices . at name ?~ varIx)

    compileStatement (Assign name ex)
    pure varIx

compileExprToReg :: (Member (Writer [Instruction]) r, CompileC r, FromSomeReg a) => Type -> Expr 'Typed -> Sem r (Register a)
compileExprToReg type_ expression = case (type_, expression) of
    (IntT, IntLit i) -> newRegForType IntT >>= \reg -> tell [MoveNumLit reg i] >> castReg reg
    (IntT, Var n) -> get <&> join . (^? frames . head1 . varIndices . at n) >>= \case
        Nothing -> panicVarNotFoundTooLate n
        Just vIx -> do
            reg <- newRegForType IntT
            frameReg <- ArrayReg <$> newReg
            varIxReg <- NumReg <$> newReg
            tell [
                  MoveNumLit varIxReg vIx
                , GetArrayInArray frameReg stackReg stackPTRReg
                , GetNumInArray reg frameReg varIxReg
                ]
            castReg reg
    -- (FloatT, FloatLit i) -> pure [MoveNumReg (NumReg reg)]
    (IntT, FCall fname args) -> do
        get <&> (^. functions . at fname) >>= \case
            Nothing -> panic' "Function not found" [show fname]
            Just f -> do
                modify (& frames %~ (emptyFrame |:))
                zipWithM_ pushVarToStack (map fst (f ^. params)) args
                tell [Call fname]
                case (f ^. returnType) of
                    Nothing -> panic' "Called a void function as an expression" [show fname]
                    Just _ -> pure returnReg -- TODO: Is this okay or does this get overriden by recursion?
                                             -- TODO: (If it is, the entire case should be removed)

    (ty, ex) -> panic' "Expression not compilable as Type" ["Type: " <> show ty, "Expr: " <> show ex]
        


{-# LANGUAGE NoImplicitPrelude, ConstraintKinds, DataKinds, LambdaCase, TemplateHaskell, OverloadedStrings#-}
{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeApplications, ViewPatterns, BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
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

panicFunNotFoundTooLate :: (Member (Error CompilerError) r) => Text -> Sem r a
panicFunNotFoundTooLate v = panic $ "Function " <> v <> " not found. This should have been caught earlier!"


data CompileState = CompileState {
        _frames :: NonEmpty Frame
      , _lastReg :: Int
      , _functions :: Map Text Function
    } deriving (Show, Eq)

initialCompileState :: CompileState
initialCompileState = CompileState {
      _frames = Frame {
        _varIndices=mempty
      , _varCount=0
      } :| []
    , _lastReg = 0
     , _functions = mempty
    }

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
compile (S.Module modname stmnts) = A.Module modname . fst <$> runWriterAssocR (traverse compileStatement stmnts)

newReg :: (CompileC r) => Sem r Int
newReg = modify (& lastReg +~ 1) >> get <&> (^. lastReg)

newRegForType :: (CompileC r, FromSomeReg a) => Type -> Sem r (Register a)
newRegForType = \case
    IntT -> castReg . NumReg =<< newReg
    EntityT -> castReg . EntityReg =<< newReg
    BoolT -> castReg . NumReg =<< newReg
    StructT _ -> castReg . ArrayReg =<< newReg

compileStatement :: forall r. (Member (Writer [Instruction]) r, CompileC r) => S.Statement 'Typed -> Sem r ()
compileStatement = \case
    Decl _ name _ expr -> void $ pushVarToStack name expr

    Assign _ name expr -> do
        varIxReg <- NumReg <$> newReg
        frameReg <- ArrayReg <$> newReg
        mvarIx <- get <&> join . (^? frames . head1 . varIndices . at name)
        case mvarIx of   
            Nothing -> panicVarNotFoundTooLate name
            Just varIx -> do
                exprReg <- fromSomeReg =<< compileExprToReg expr
                tell [
                      GetArrayInArray frameReg stackReg stackPTRReg
                    , MoveNumLit varIxReg varIx
                    , SetNumInArray frameReg varIxReg exprReg
                    ]
    CallFun _ name args -> get <&> (^? functions . ix name . returnType . _Just) >>= \case
        Nothing -> panicFunNotFoundTooLate name
        Just rt -> void $ compileExprToReg (FCall rt name args)

    DefVoid _ name pars body -> do
        modify (& functions . at name ?~ Function {_params=pars, _returnType=Nothing})
        tell . pure . A.Section name . fst =<< runWriterAssocR do
            modify (& frames . head1 . varIndices .~ fromList (zip (map fst pars) [0..]))
            modify (& frames . head1 . varCount .~ length pars)
            traverse_ compileStatement body

pushVarToStack :: (Member (Writer [Instruction]) r, CompileC r) => Text -> Expr 'Typed -> Sem r Int
pushVarToStack name ex = do
    varIx <- get <&> (^. frames . head1 . varCount)
    modify (& frames . head1 . varCount +~ 1)
    modify (& frames . head1 . varIndices . at name ?~ varIx)

    compileStatement (Assign void_ name ex)
    pure varIx

compileExprToReg :: (Member (Writer [Instruction]) r, CompileC r) => Expr 'Typed -> Sem r SomeReg
compileExprToReg = \case
    (IntLit _ i) -> newRegForType IntT >>= \reg -> tell [MoveNumLit reg i] $> SomeReg reg
    (BoolLit _ b) -> newRegForType BoolT >>= \reg -> tell [MoveNumLit reg (bool 0 1 b)] $> SomeReg reg
    (Var t n) -> get <&> join . (^? frames . head1 . varIndices . at n) >>= \case
        Nothing -> panicVarNotFoundTooLate n
        Just vIx -> do
            reg <- newRegForType t
            frameReg <- ArrayReg <$> newReg
            varIxReg <- NumReg <$> newReg
            tell [
                  MoveNumLit varIxReg vIx
                , GetArrayInArray frameReg stackReg stackPTRReg
                , GetNumInArray reg frameReg varIxReg
                ]
            pure $ SomeReg reg
    -- (FloatT, FloatLit i) -> pure [MoveNumReg (NumReg reg)]
    (FCall t fname args) -> do
        get <&> (^. functions . at fname) >>= \case
            Nothing -> panicFunNotFoundTooLate fname
            Just f -> do
                modify (& frames %~ (emptyFrame |:))
                zipWithM_ pushVarToStack (map fst (f ^. params)) args
                tell [Call fname]
                case (f ^. returnType) of
                    Nothing -> panic' "Called a void function as an expression" [show fname]
                    Just _ -> pure $ SomeReg returnReg -- TODO: Is this okay or does this get overriden by recursion?
                                                       -- TODO: (If it is, the entire case should be removed)    
                                                       -- TODO: PROBABLY BROKEN!! (returnReg is polymorphic, but
                                                       -- TODO registers for different types are implemented differently)

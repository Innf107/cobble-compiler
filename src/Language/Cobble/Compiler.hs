{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Compiler where

import Language.Cobble.Prelude

import Language.Cobble.Util

import Language.Cobble.Types as S

import Language.Cobble.MCAsm.Types as A hiding (Name)

type CompileC r = Members '[State CompileState, Error Panic, Error McAsmError, Output Log] r

panic :: (Member (Error Panic) r) => Text -> Sem r a
panic = throw . Panic

panic' :: (Member (Error Panic) r) => Text -> [Text] -> Sem r a
panic' t as = panic $ t <> "\n\nContext: \n" <> unlines (map ("    "<>) as)

panicVarNotFoundTooLate :: (Member (Error Panic) r) => Name 'Codegen -> Sem r a
panicVarNotFoundTooLate v = panic $ "Variable " <> show v <> " not found. This should have been caught earlier!" 

panicFunNotFoundTooLate :: (Members [Error Panic, State CompileState] r) => Name 'Codegen -> Sem r a
panicFunNotFoundTooLate v = do
    st <- get
    panic $ "Function " <> show v <> " not found. This should have been caught earlier!\n    State: " <> show st

data CompileState = CompileState {
        _frames :: NonEmpty Frame
      , _functions :: Map (Name 'Codegen) Function
    } deriving (Show, Eq)

initialCompileState :: CompileState
initialCompileState = CompileState {
      _frames = Frame {
        _varRegs=mempty
      , _lastReg=0
      } :| []
    , _functions = mempty 
    }

data Frame = Frame {
        _varRegs :: Map (Name 'Codegen) Register
    ,   _lastReg :: Int
    } deriving (Show, Eq)

data Function = Function {
      _params :: [(Name 'Codegen, Type 'Codegen)]
    , _returnType :: Maybe (Type 'Codegen)
    } deriving (Show, Eq)

emptyFrame :: Frame
emptyFrame = Frame mempty 0

stackReg :: Register
stackReg = ArrayReg $ NamedReg "STACK"

stackPTRReg :: Register
stackPTRReg = NumReg $ NamedReg "STACKPTR"


makeLenses 'Frame
makeLenses 'CompileState
makeLenses 'Function

rts :: Sem r A.Module
rts = do
    pure $ A.Module "RTS" [
          MoveNumLit stackPTRReg 0 
        ]

compile :: (CompileC r) => S.Module 'Codegen -> Sem r A.Module
compile (S.Module _deps modname stmnts) = log LogVerbose ("STARTING COBBLE CODEGEN FOR MODULE: " <> show modname) 
                                       >> A.Module modname . fst <$> runWriterAssocR (traverse compileStatement stmnts)

newReg :: (CompileC r) => (Int -> RegId) -> Sem r RegId
newReg c = (get <&> c . (^. frames . head1 . lastReg)) <* modify (& frames . head1 . lastReg +~ 1)

newRegForType :: (CompileC r) => (Int -> RegId) -> Type 'Codegen -> Sem r Register
newRegForType c t = case rtType t of
    RepNum    -> NumReg    <$> newReg c
    RepEntity -> EntityReg <$> newReg c
    RepArray  -> ArrayReg  <$> newReg c

compileStatement :: forall r. (Member (Writer [Instruction]) r, CompileC r) => S.Statement 'Codegen -> Sem r ()
compileStatement s = (log LogDebugVerbose ("COMPILING STATEMENT: " <>  show s) >>) $ s & \case
    Decl () li name _ expr -> do
        nr <- newRegForType VarReg (getType expr)
        modify (& frames . head1 . varRegs . at name ?~ nr)

        compileStatement (AssignT li name expr)

    Assign () _li name expr -> do
        mvarReg <- get <&> join . (^? frames . head1 . varRegs . at name)
        case mvarReg of
            Nothing -> panicVarNotFoundTooLate name
            Just varReg -> do
                exprReg <- compileExprToReg expr
                tell [MoveReg varReg exprReg]
                
    CallFun () li fname args -> get <&> (^? functions . ix fname . returnType . _Just) >>= \case
        Nothing -> get <&> (^? functions . ix fname . params) >>= \case
            Just ps -> do
                frame <- gets (^. frames . head1)
                saveFrame frame
                tell [Call fname]
                restoreFrame frame
            Nothing -> panicFunNotFoundTooLate fname
        Just rt -> void $ compileExprToReg (FCall rt li fname args)


    DefVoid () _li name pars body -> do
        modify (& functions . at name ?~ Function {_params=pars, _returnType=Nothing})
        tell . pure . A.Section name . fst =<< runWriterAssocR do
            modify (& frames %~ (emptyFrame |:))
            regs <- traverse (\(_, t) -> newRegForType VarReg t) pars
            modify (& frames . head1 . varRegs .~ fromList (zip (map fst pars) regs))
            traverse_ compileStatement body
            modify (& frames %~ unsafeTail)
    DefFun () _li name pars body retExp t -> do
        modify (& functions . at name ?~ Function {_params=pars, _returnType=Just t})
        tell . pure . A.Section name . fst =<< runWriterAssocR do
            regs <- traverse (\(_, pt) -> newRegForType VarReg pt) pars
            modify (& frames %~ (emptyFrame |:))
            modify (& frames . head1 . varRegs .~ fromList (zip (map fst pars) regs))
            traverse_ compileStatement body
            modify (& frames %~ unsafeTail)
            res <- compileExprToReg retExp
            tell [MoveReg (returnReg (regRep res)) res]
    S.SetScoreboard () _li obj player ex -> do
        r <- compileExprToReg ex
        tell [A.SetScoreboard obj player r]

compileExprToReg :: (Member (Writer [Instruction]) r, CompileC r) => Expr 'Codegen -> Sem r Register
compileExprToReg e = (log LogDebugVerbose ("COMPILING EXPR: " <> show e) >>) $ e & \case
    (IntLit () _li i)  -> NumReg <$> newReg TempReg >>= \reg -> tell [MoveNumLit reg i] $> reg
    (BoolLit () _li b) -> NumReg <$> newReg TempReg >>= \reg -> tell [MoveNumLit reg (bool 0 1 b)] $> reg
    (Var t _li n) -> get <&> join . (^? frames . head1 . varRegs . at n) >>= \case
        Nothing -> panicVarNotFoundTooLate n
        Just vReg -> do
            retReg <- newRegForType TempReg t
            tell [MoveReg retReg vReg]
            pure $ retReg
    -- (FloatT, FloatLit i) -> pure [MoveNumReg (NumReg reg)]
    (FCall t _li fname args) -> do
        get <&> (^. functions . at fname) >>= \case
            Nothing -> panicFunNotFoundTooLate fname
            Just f -> do
                frame <- gets (^. frames . head1)
                saveFrame frame
                
                writeArgs args

                tell [Call fname]

                restoreFrame frame
                case (f ^. returnType) of
                    Nothing -> panic' "Called a void function as an expression" [show fname]
                    Just t' | t' /= t -> panic' "Return type of function does not match fcall expr return type" [show fname, show t, show t']
                    Just _ -> pure $ returnReg (rtType t)
    ExprX x _li -> absurd x

writeArgs :: (CompileC r, Member (Writer [Instruction]) r) => [Expr 'Codegen] -> Sem r ()
writeArgs args = do
    ress <- traverse compileExprToReg args
    zipWithM_ (\r i -> tell [MoveReg (mkRegFromRep (regRep r) (VarReg i)) r]) ress [0..]

-- TODO: Initialize empty stack elements and write with `SetInArray`
saveFrame :: (CompileC r, Member (Writer [Instruction]) r) => Frame -> Sem r ()
saveFrame f = traverse_ pushRegToStack (toList $ f ^. varRegs)

restoreFrame :: (CompileC r, Member (Writer [Instruction]) r) => Frame -> Sem r ()
restoreFrame f = traverse_ popRegFromStack (reverse $ toList $ f ^. varRegs)

pushRegToStack :: (CompileC r, Member (Writer [Instruction]) r) => Register -> Sem r()
pushRegToStack r = tell [
        SetInArrayOrNew stackReg stackPTRReg r
    ,   AddLit stackPTRReg 1
    ]
      
popRegFromStack :: (CompileC r, Member (Writer [Instruction]) r) => Register -> Sem r()
popRegFromStack r = tell [
        SubLit stackPTRReg 1
    ,   GetInArray r stackReg stackPTRReg
    ,   DestroyInArray stackReg stackPTRReg
    ]

returnReg :: Rep -> Register
returnReg = \case
    RepNum    -> NumReg    (NamedReg "RETURN")
    RepEntity -> EntityReg (NamedReg "RETURN")
    RepArray  -> ArrayReg  (NamedReg "RETURN")

rtType :: Type 'Codegen -> Rep
rtType = \case
    TCon "prims.Int" KStar    -> RepNum
    TCon "prims.Bool" KStar   -> RepNum
    TCon "prims.Entity" KStar -> RepEntity
    _                   -> RepArray

regRep :: Register -> Rep
regRep = \case
    NumReg _ -> RepNum
    EntityReg _ -> RepEntity
    ArrayReg _ -> RepArray

regId :: Register -> RegId
regId = \case
    NumReg r -> r
    EntityReg r -> r
    ArrayReg r -> r

mkRegFromRep :: Rep -> RegId -> Register
mkRegFromRep r i = case r of
    RepNum    -> NumReg i
    RepEntity -> EntityReg i
    RepArray  -> ArrayReg i


data Rep = RepNum | RepEntity | RepArray deriving (Show, Eq)

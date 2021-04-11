{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Compiler where

import Language.Cobble.Prelude

import Language.Cobble.Types as S

import Language.Cobble.MCAsm.Types as A hiding (Name)

type CompileC r = Members '[State CompileState, Error Panic, Error McAsmError, Output Log] r

panic :: (Member (Error Panic) r) => Text -> Sem r a
panic = throw . Panic

panic' :: (Member (Error Panic) r) => Text -> [Text] -> Sem r a
panic' t as = panic $ t <> "\n\nContext: \n" <> unlines (map ("    "<>) as)

panicVarNotFoundTooLate :: (Member (Error Panic) r) => Name 'Codegen -> Sem r a
panicVarNotFoundTooLate v = panic $ "Variable " <> show v <> " not found. This should have been caught earlier!" 

panicFunNotFoundTooLate :: (Member (Error Panic) r) => Name 'Codegen -> Sem r a
panicFunNotFoundTooLate v = panic $ "Function " <> show v <> " not found. This should have been caught earlier!"

data CompileState = CompileState {
        _frames :: NonEmpty Frame
      , _lastReg :: Int
      , _functions :: Map (Name 'Codegen) Function
    } deriving (Show, Eq)

initialCompileState :: CompileState
initialCompileState = CompileState {
      _frames = Frame {
        _varRegs=mempty
      } :| []
    , _lastReg = 0
     , _functions = mempty
    }

data Frame = Frame {
        _varRegs :: Map (Name 'Codegen) Register
    } deriving (Show, Eq)

data Function = Function {
      _params :: [(Name 'Codegen, Type 'Codegen)]
    , _returnType :: Maybe (Type 'Codegen)
    } deriving (Show, Eq)

emptyFrame :: Frame
emptyFrame = Frame mempty

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

newReg :: (CompileC r) => Sem r RegId
newReg = modify (& lastReg +~ 1) >> get <&> IdReg . (^. lastReg)

newRegForType :: (CompileC r) => Type 'Codegen -> Sem r Register
newRegForType t = case rtType t of
    RepNum    -> NumReg    <$> newReg
    RepEntity -> EntityReg <$> newReg
    RepArray  -> ArrayReg  <$> newReg

compileStatement :: forall r. (Member (Writer [Instruction]) r, CompileC r) => S.Statement 'Codegen -> Sem r ()
compileStatement s = (log LogDebugVerbose ("COMPILING STATEMENT: " <>  show s) >>) $ s & \case
    Decl () _li name _ expr -> void $ pushVarToStack name expr

    Assign () _li name expr -> do
        mvarReg <- get <&> join . (^? frames . head1 . varRegs . at name)
        case mvarReg of
            Nothing -> panicVarNotFoundTooLate name
            Just varReg -> do
                exprReg <- compileExprToReg expr
                tell [MoveReg varReg exprReg]
                
    CallFun () li name args -> get <&> (^? functions . ix name . returnType . _Just) >>= \case
        Nothing -> panicFunNotFoundTooLate name
        Just rt -> void $ compileExprToReg (FCall rt li name args)

    DefVoid () _li name pars body -> do
        modify (& functions . at name ?~ Function {_params=pars, _returnType=Nothing})
        tell . pure . A.Section name . fst =<< runWriterAssocR do
            regs <- traverse (\(_, t) -> newRegForType t) pars
            modify (& frames . head1 . varRegs .~ fromList (zip (map fst pars) regs))
            traverse_ compileStatement body
    DefFun () _li name pars body retExp t -> do
        modify (& functions . at name ?~ Function {_params=pars, _returnType=Just t})
        tell . pure . A.Section name . fst =<< runWriterAssocR do
            regs <- traverse (\(_, pt) -> newRegForType pt) pars
            modify (& frames . head1 . varRegs .~ fromList (zip (map fst pars) regs))
            traverse_ compileStatement body
            res <- compileExprToReg retExp
            tell [MoveReg (returnReg (regRep res)) res]
    S.SetScoreboard () _li obj player ex -> do
        r <- compileExprToReg ex
        tell [A.SetScoreboard obj player r]


pushVarToStack :: (Member (Writer [Instruction]) r, CompileC r) => Name 'Codegen -> Expr 'Codegen -> Sem r Int
pushVarToStack name ex = do
    varIx <- get <&> ((^. frames . head1 . varRegs) >>> length)
    nr <- newRegForType (getType ex)
    modify (& frames . head1 . varRegs . at name ?~ nr)

    compileStatement (AssignT (LexInfo 0 0 "YouShouldNotSeeThis!") name ex)
    pure varIx

compileExprToReg :: (Member (Writer [Instruction]) r, CompileC r) => Expr 'Codegen -> Sem r Register
compileExprToReg e = (log LogDebugVerbose ("COMPILING EXPR: " <> show e) >>) $ e & \case
    (IntLit () _li i)  -> NumReg <$> newReg >>= \reg -> tell [MoveNumLit reg i] $> reg
    (BoolLit () _li b) -> NumReg <$> newReg >>= \reg -> tell [MoveNumLit reg (bool 0 1 b)] $> reg
    (Var t _li n) -> get <&> join . (^? frames . head1 . varRegs . at n) >>= \case
        Nothing -> panicVarNotFoundTooLate n
        Just vReg -> do
            retReg <- newRegForType t
            tell [MoveReg retReg vReg]
            pure $ retReg
    -- (FloatT, FloatLit i) -> pure [MoveNumReg (NumReg reg)]
    (FCall t _li fname args) -> do
        frame <- gets (^. frames . head1)
        saveFrame frame

        tell [Call fname]
        
        restoreFrame frame
        
        get <&> (^. functions . at fname) >>= \case
            Nothing -> panicFunNotFoundTooLate fname
            Just f -> do
                modify (& frames %~ (emptyFrame |:))
                zipWithM_ pushVarToStack (map fst (f ^. params)) args
                tell [Call fname]
                case (f ^. returnType) of
                    Nothing -> panic' "Called a void function as an expression" [show fname]
                    Just t' | t' /= t -> panic' "Return type of function does not match fcall expr return type" [show fname, show t, show t']
                    Just _ -> pure $ returnReg (rtType t)
    ExprX x _li -> absurd x

saveFrame :: (CompileC r, Member (Writer [Instruction]) r) => Frame -> Sem r ()
saveFrame f = traverse_ pushRegToStack (toList $ f ^. varRegs)

restoreFrame :: (CompileC r, Member (Writer [Instruction]) r) => Frame -> Sem r ()
restoreFrame f = traverse_ popRegFromStack (reverse $ toList $ f ^. varRegs)

pushRegToStack :: (CompileC r, Member (Writer [Instruction]) r) => Register -> Sem r()
pushRegToStack r = tell [
        SetInArray stackReg stackPTRReg r
    ,   AddLit stackPTRReg 1
    ]
      
popRegFromStack :: (CompileC r, Member (Writer [Instruction]) r) => Register -> Sem r()
popRegFromStack r = tell [
        SubLit stackPTRReg 1
    ,   GetInArray r stackReg stackPTRReg
    ]

returnReg :: Rep -> Register
returnReg = \case
    RepNum    -> NumReg    (NamedReg "RETURN")
    RepEntity -> EntityReg (NamedReg "RETURN")
    RepArray  -> ArrayReg  (NamedReg "RETURN")

rtType :: Type 'Codegen -> Rep
rtType = \case
    TCon "Prelude.Int" KStar    -> RepNum
    TCon "Prelude.Bool" KStar   -> RepNum
    TCon "Prelude.Entity" KStar -> RepEntity
    _                           -> RepArray

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

data Rep = RepNum | RepEntity | RepArray deriving (Show, Eq)

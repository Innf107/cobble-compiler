{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Compiler where

import Language.Cobble.Prelude

import Language.Cobble.Types as S

import Language.Cobble.MCAsm.Types as A hiding (Name)

type CompileC r = Members '[State CompileState, Error Panic, Error McAsmError] r

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
        _varIndices=mempty
      , _varCount=0
      } :| []
    , _lastReg = 0
     , _functions = mempty
    }

data Frame = Frame {
        _varIndices :: Map (Name 'Codegen) Int
      , _varCount :: Int
    } deriving (Show, Eq)

data Function = Function {
      _params :: [(Name 'Codegen, Type 'Codegen)]
    , _returnType :: Maybe (Type 'Codegen)
    } deriving (Show, Eq)

emptyFrame :: Frame
emptyFrame = Frame mempty 0

stackReg :: Register 'Array
stackReg = ArrayReg $ NamedReg "STACK"

stackPTRReg :: Register 'Number
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
compile (S.Module _deps modname stmnts) = A.Module modname . fst <$> runWriterAssocR (traverse compileStatement stmnts)

newReg :: (CompileC r) => Sem r RegId
newReg = modify (& lastReg +~ 1) >> get <&> IdReg . (^. lastReg)

newRegForType :: (CompileC r, FromSomeReg a) => Type 'Codegen -> Sem r (Register a)
newRegForType t = case rtType t of
    Number -> castReg . NumReg    =<< newReg
    Entity -> castReg . EntityReg =<< newReg
    Array  -> castReg . ArrayReg  =<< newReg

compileStatement :: forall r. (Member (Writer [Instruction]) r, CompileC r) => S.Statement 'Codegen -> Sem r ()
compileStatement = \case
    Decl () _li name _ expr -> void $ pushVarToStack name expr

    Assign () _li name expr -> do
        varIxReg <- NumReg <$> newReg
        frameReg <- ArrayReg <$> newReg
        mvarIx <- get <&> join . (^? frames . head1 . varIndices . at name)
        case mvarIx of
            Nothing -> panicVarNotFoundTooLate name
            Just varIx -> do
                exprReg <- compileExprToReg expr
                tell [
                      GetInArray frameReg stackReg stackPTRReg
                    , MoveNumLit varIxReg varIx
                    ]
                tell . pure =<< makeInstr (getType expr) exprReg (\r -> SetInArray frameReg varIxReg r)

    CallFun () li name args -> get <&> (^? functions . ix name . returnType . _Just) >>= \case
        Nothing -> panicFunNotFoundTooLate name
        Just rt -> void $ compileExprToReg (FCall rt li name args)

    DefVoid () _li name pars body -> do
        modify (& functions . at name ?~ Function {_params=pars, _returnType=Nothing})
        tell . pure . A.Section name . fst =<< runWriterAssocR do
            modify (& frames . head1 . varIndices .~ fromList (zip (map fst pars) [0..]))
            modify (& frames . head1 . varCount .~ length pars)
            traverse_ compileStatement body
    DefFun () _li name pars body retExp t -> do
        modify (& functions . at name ?~ Function {_params=pars, _returnType=Just t})
        tell . pure . A.Section name . fst =<< runWriterAssocR do
            modify (& frames . head1 . varIndices .~ fromList (zip (map fst pars) [0..]))
            modify (& frames . head1 . varCount .~ length pars)
            traverse_ compileStatement body
            res <- compileExprToReg retExp
            tell .pure =<< makeInstr t res (\res' -> MoveReg returnReg res')
    S.SetScoreboard () _li obj player ex -> do
        r <- fromSomeReg =<< compileExprToReg ex
        tell [A.SetScoreboard obj player r]


pushVarToStack :: (Member (Writer [Instruction]) r, CompileC r) => Name 'Codegen -> Expr 'Codegen -> Sem r Int
pushVarToStack name ex = do
    varIx <- get <&> (^. frames . head1 . varCount)
    modify (& frames . head1 . varCount +~ 1)
    modify (& frames . head1 . varIndices . at name ?~ varIx)

    compileStatement (AssignT (LexInfo 0 0 "ThisShouldNotHaveHappened!") name ex)
    pure varIx

compileExprToReg :: (Member (Writer [Instruction]) r, CompileC r) => Expr 'Codegen -> Sem r SomeReg
compileExprToReg = \case
    (IntLit () _li i)  -> NumReg <$> newReg >>= \reg -> tell [MoveNumLit reg i] $> SomeReg reg
    (BoolLit () _li b) -> NumReg <$> newReg >>= \reg -> tell [MoveNumLit reg (bool 0 1 b)] $> SomeReg reg
    (Var t _li n) -> get <&> join . (^? frames . head1 . varIndices . at n) >>= \case
        Nothing -> panicVarNotFoundTooLate n
        Just vIx -> do
            regID <- newReg
            frameReg <- ArrayReg <$> newReg
            varIxReg <- NumReg <$> newReg
            tell [
                  MoveNumLit varIxReg vIx
                , GetInArray @'Array frameReg stackReg stackPTRReg
                ]
            makeInstr' t regID \r -> do
                tell [GetInArray r frameReg varIxReg]
                pure $ SomeReg r
    -- (FloatT, FloatLit i) -> pure [MoveNumReg (NumReg reg)]
    (FCall t _li fname args) -> do
        get <&> (^. functions . at fname) >>= \case
            Nothing -> panicFunNotFoundTooLate fname
            Just f -> do
                modify (& frames %~ (emptyFrame |:))
                zipWithM_ pushVarToStack (map fst (f ^. params)) args
                tell [Call fname]
                case (f ^. returnType) of
                    Nothing -> panic' "Called a void function as an expression" [show fname]
                    Just t' | t' /= t -> panic' "Return type of function does not match fcall expr return type" [show fname, show t, show t']
                    Just _ -> case rtType t of
                        Number -> pure $ SomeReg @'Number returnReg
                        Entity -> pure $ SomeReg @'Entity returnReg
                        Array  -> pure $ SomeReg @'Array  returnReg
    ExprX x _li -> absurd x

moveReg :: (CompileC r, Member (Writer [Instruction]) r) => Type 'Codegen -> SomeReg -> SomeReg -> Sem r ()
moveReg t r1 r2 = case t of
    TCon "Int" KStar  -> MoveReg @'Number <$> (fromSomeReg r1) <*> (fromSomeReg r2) >>= tell . pure
    TCon "Bool" KStar -> MoveReg @'Number <$> (fromSomeReg r1) <*> (fromSomeReg r2) >>= tell . pure
    _ -> error "TODO: moveReg for arbitrary types (MoveArray)"

makeInstr :: (CompileC r) => Type 'Codegen -> SomeReg -> (forall rt. (ObjForType rt, ReturnReg rt) => Register rt -> a) -> Sem r a
makeInstr t r f = case rtType t of
    Number -> f <$> fromSomeReg @'Number r
    Entity -> f <$> fromSomeReg @'Entity r
    Array  -> f <$> fromSomeReg @'Array  r

makeInstr' :: Type 'Codegen -> RegId -> (forall rt. (ObjForType rt, ReturnReg rt) => Register rt -> a) -> a
makeInstr' t rid f = case rtType t of
    Number -> f $ NumReg rid
    Entity -> f $ EntityReg rid
    Array  -> f $ ArrayReg rid



rtType :: Type 'Codegen -> RegType
rtType = \case
    TCon "Int" KStar -> Number
    TCon "Bool" KStar -> Number
    TCon "Entity" KStar -> Entity
    _ -> Array

regId :: Register t -> RegId
regId = \case
    NumReg r -> r
    EntityReg r -> r
    ArrayReg r -> r

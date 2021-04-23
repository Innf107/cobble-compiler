{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Compiler where

import Language.Cobble.Prelude

import Language.Cobble.Util

import Language.Cobble.Types as S

import Language.Cobble.Codegen.Types hiding (PrimOpEnv(..))
import Language.Cobble.Codegen.Types qualified as P (PrimOpEnv(..))
import Language.Cobble.Codegen.PrimOps

import Data.Text qualified as T

import Language.Cobble.MCAsm.Types as A hiding (Name)

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

initialCompileState :: CompileState
initialCompileState = CompileState {
      _frames = Frame {
        _varRegs=mempty
      , _lastReg=0
      , _regs=[]
      } :| []
    , _functions = mempty 
    }


emptyFrame :: Frame
emptyFrame = Frame mempty 0 []

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

newReg :: (CompileC r) => (Int -> RegId) -> (RegId -> Register) -> Sem r Register
newReg c r = do
    reg <- get <&> r . c . (^. frames . head1 . lastReg)
    modify (& frames . head1 . lastReg +~ 1)
    modify (& frames . head1 . regs %~ (reg:))
    pure reg

newRegForType :: (CompileC r) => (Int -> RegId) -> Type 'Codegen -> Sem r Register
newRegForType c t = case rtType t of
    RepNum    -> newReg c NumReg
    RepEntity -> newReg c EntityReg
    RepArray  -> newReg c ArrayReg

compileStatement :: forall r. (Member (Writer [Instruction]) r, CompileC r) => S.Statement 'Codegen -> Sem r ()
compileStatement s = (log LogDebugVerbose ("COMPILING STATEMENT: " <>  show s) >>) $ s & \case
    Decl () li name _ expr -> do
        nr <- newRegForType VarReg (getType expr)
        modify (& frames . head1 . varRegs . at name ?~ nr)

        compileStatement (Assign () li name expr)

    Assign () _li name expr -> do
        mvarReg <- get <&> join . (^? frames . head1 . varRegs . at name)
        case mvarReg of
            Nothing -> panicVarNotFoundTooLate name
            Just varReg -> do
                exprReg <- compileExprToReg expr
                tell [MoveReg varReg exprReg]

    IfS (name, ifID) _li c th mel -> do
        cr <- compileExprToReg c
        let elPostfix = maybe [] (const [MoveNumLit elseReg 0]) mel
        tell . pure . A.Section (name .: ("-then" <> show ifID)) =<< (<>elPostfix) . fst <$> runWriterAssocR (traverse compileStatement th)
        case mel of
            Just el -> do
                tell . pure . A.Section (name .: ("-else" <> show ifID)) =<< fst <$> runWriterAssocR (traverse compileStatement el)
                tell [MoveNumLit elseReg 1
                    , CallInRange cr (RBounded 1 1) (name .: ("-then" <> show ifID))
                    , CallInRange elseReg (RBounded 1 1) (name .: ("-else" <> show ifID))
                    ]
            Nothing -> do
                tell [CallInRange cr (RBounded 1 1) (name .: ("-then" <> show ifID))]

    CallFun () li f@(Var _vt _vli fname) args -> case lookup fname primOps of
        Just (_, _, primOpF) -> void $ primOpF primOpEnv args
        Nothing -> gets (^? functions . ix fname . returnType . _Just) >>= \case
            Nothing -> get <&> (^? functions . ix fname . params) >>= \case
                Just ps -> do
                    frame <- gets (^. frames . head1)
                    saveFrame frame
                    writeArgs args
                    tell [Call fname]
                    restoreFrame frame
                Nothing -> panicFunNotFoundTooLate fname
            Just rt -> void $ compileExprToReg (FCall rt li f args)

    CallFun () li ex _args -> panic' "Cannot indirectly call a function yet. This is *NOT* a bug" [show ex, show li]

    DefVoid () _li name pars body -> do
        modify (& functions . at name ?~ Function {_params=pars, _returnType=Nothing})
        tell . pure . A.Section name . fst =<< runWriterAssocR do
            modify (& frames %~ (emptyFrame |:))
            parRegs <- traverse (\(_, t) -> newRegForType VarReg t) pars
            modify (& frames . head1 . varRegs .~ fromList (zip (map fst pars) parRegs))
            modify (& frames . head1 . regs .~ parRegs)
            traverse_ compileStatement body
            modify (& frames %~ unsafeTail)
    DefFun () _li name pars body retExp t -> do
        modify (& functions . at name ?~ Function {_params=pars, _returnType=Just t})
        tell . pure . A.Section name . fst =<< runWriterAssocR do
            parRegs <- traverse (\(_, pt) -> newRegForType VarReg pt) pars
            modify (& frames %~ (emptyFrame |:))
            modify (& frames . head1 . varRegs .~ fromList (zip (map fst pars) parRegs))
            modify (& frames . head1 . regs .~ parRegs)
            traverse_ compileStatement body
            res <- compileExprToReg retExp
            modify (& frames %~ unsafeTail)
            tell [MoveReg (returnReg (regRep res)) res]
    S.SetScoreboard () _li obj player ex -> do
        r <- compileExprToReg ex
        tell [A.SetScoreboard obj player r]
    LogS _li segs -> do
        segs' <- traverse renderLogSeg segs
        tell [A.RawCommand $ "tellraw @a [" <> T.intercalate ", " segs' <> "]"]
    Import () _ _ -> pass
    DefStruct () _ _ _ -> pass
    StatementX x _ -> absurd x

renderLogSeg :: (CompileC r) => LogSegment 'Codegen -> Sem r Text
renderLogSeg (LogText t) = pure $ "{\"text\":\""<> t <>"\"}"
renderLogSeg (LogVar v) = get <&> join . (^? frames . head1 . varRegs . at v) >>= \case
                                  Nothing -> panicVarNotFoundTooLate v
                                  Just vReg -> pure $ T.intercalate "," $ ["REGS", "EPTR", "APTR"] & map (\s ->
                                    "{\"score\":{\"name\":\""<> renderReg vReg <>"\",\"objective\":\"" <> s <> "\"}}")

compileExprToReg :: (Member (Writer [Instruction]) r, CompileC r) => Expr 'Codegen -> Sem r Register
compileExprToReg e = (log LogDebugVerbose ("COMPILING EXPR: " <> show e) >>) $ e & \case
    (IntLit () _li i)  -> newReg TempReg NumReg >>= \reg -> tell [MoveNumLit reg i] $> reg
    (BoolLit () _li b) -> newReg TempReg NumReg >>= \reg -> tell [MoveNumLit reg (bool 0 1 b)] $> reg
    (Var t _li n) -> get <&> join . (^? frames . head1 . varRegs . at n) >>= \case
        Nothing -> panicVarNotFoundTooLate n
        Just vReg -> do
            retReg <- newRegForType TempReg t
            tell [MoveReg retReg vReg]
            pure $ retReg
    -- (FloatT, FloatLit i) -> pure [MoveNumReg (NumReg reg)]
    (FCall t _li (Var _ _vli fname) args) -> case lookup fname primOps of
        Just (_, _, primOpF) -> primOpF primOpEnv args
        Nothing -> do
            get <&> (^. functions . at fname) >>= \case
                Nothing -> panicFunNotFoundTooLate fname
                Just f -> do
                    frame <- gets (^. frames . head1)
                    saveFrame frame

                    writeArgs args

                    tell [Call fname]

                    restoreFrame frame
                    
                    ret <- newRegForType TempReg t
                    tell [MoveReg ret (returnReg (rtType t))]
                    case (f ^. returnType) of
                        Nothing -> panic' "Called a void function as an expression" [show fname]
                        Just t' | t' /= t -> panic' "Return type of function does not match fcall expr return type" [show fname, show t, show t']
                        Just _ -> pure $ ret
    IfE (name, ifID) _li c th el -> do
        cr <- compileExprToReg c
        resReg <- newRegForType TempReg (getType th)
        tell . pure . A.Section (name .: ("-then-e" <> show ifID)) =<< (\(is, r) -> is <> [MoveNumLit elseReg 0, MoveReg resReg r]) <$> runWriterAssocR (compileExprToReg th)
        tell . pure . A.Section (name .: ("-else-e" <> show ifID)) =<< (\(is, r) -> is <> [MoveReg resReg r]) <$> runWriterAssocR (compileExprToReg el)
        tell [MoveNumLit elseReg 1
            , CallInRange cr (RBounded 1 1) (name .: ("-then-e" <> show ifID))
            , CallInRange elseReg (RBounded 1 1) (name .: ("-else-e" <> show ifID))
            ]
        pure resReg
                
    ExprX x _li -> absurd x

writeArgs :: (CompileC r, Member (Writer [Instruction]) r) => [Expr 'Codegen] -> Sem r ()
writeArgs args = do
    ress <- traverse compileExprToReg args
    zipWithM_ (\r i -> tell [MoveReg (mkRegFromRep (regRep r) (VarReg i)) r]) ress [0..]

-- TODO: Initialize empty stack elements and write with `SetInArray`
saveFrame :: (CompileC r, Member (Writer [Instruction]) r) => Frame -> Sem r ()
saveFrame f = traverse_ pushRegToStack (toList $ f ^. regs)

restoreFrame :: (CompileC r, Member (Writer [Instruction]) r) => Frame -> Sem r ()
restoreFrame f = traverse_ popRegFromStack (reverse $ toList $ f ^. regs)

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

elseReg :: Register
elseReg = NumReg (NamedReg "ELSE")

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

primOpEnv :: (Member (Writer [Instruction]) r, CompileC r) => P.PrimOpEnv r
primOpEnv = P.PrimOpEnv {
        compileExprToReg
    ,   newReg
    }

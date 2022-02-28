module Language.Cobble.Codegen.CoreToRacket where

import Language.Cobble.Prelude
import Language.Cobble.Core.Types as F
import Language.Cobble.Racket.Types as R

compile :: [Decl] -> Sem r [RacketExpr] 
compile [] = pure []
compile (Def x _ty e : ds) = (:) 
    <$> (RDefine x <$> compileExpr e) 
    <*> compile ds


compileExpr :: Expr -> Sem r RacketExpr
compileExpr (Var x) = pure $ RVar x
compileExpr (App e1 e2) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    pure (RApp e1' [e2'])
compileExpr (TyApp e _ty) = compileExpr e
compileExpr (Abs x _ty e) = do
    e' <- compileExpr e
    pure (RLambda [x] [e'])
compileExpr (TyAbs _ _ e) = compileExpr e
compileExpr (IntLit n) = pure $ RIntLit n
compileExpr UnitLit = pure $ RNil
compileExpr (Let x _ e1 e2) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    pure $ RLet [(x, e1')] [e2']
compileExpr (If c th el) = RIf
    <$> compileExpr c
    <*> compileExpr th
    <*> compileExpr el

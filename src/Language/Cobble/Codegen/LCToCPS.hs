module Language.Cobble.Codegen.LCToCPS where

import Language.Cobble.Prelude
import Language.Cobble.LC.Types as L
import Language.Cobble.CPS.Types as C

compile :: (Member (State Int) r) => LCDef -> Sem r CPSDef
compile (LCDef name expr) = CPSDef name <$> compileExpr expr pure

freshVar :: (Member (State Int) r) => CPSVar -> Sem r CPSVar
freshVar v = state (\i -> (v <> show i, i + 1))

compileExpr :: (Member (State Int) r) => LCExpr -> (CPSExpr -> Sem r CPSExpr) -> Sem r CPSExpr
compileExpr (L.Var v) k             = k (C.Var v)
compileExpr (L.Lambda p body) k     = do
    c <- freshVar "c"
    body' <- compileTailCall body (C.Var c)
    k (C.Lambda p (C.Lambda c body'))
compileExpr (L.App fun arg) k       = do
    r <- freshVar "r"
    rest <- k (C.Var r)
    compileExpr fun (\f -> compileExpr arg \a -> pure (C.App (C.App f a) (C.Lambda r rest)))
compileExpr (L.IntLit i) k          = k (C.IntLit i)
compileExpr (L.Tuple xs) k          = compileTuple xs k
compileExpr (L.SelectTuple i t) k   = compileExpr t \t' -> k (C.SelectTuple i t')

compileTuple :: (Member (State Int) r) => [LCExpr] -> (CPSExpr -> Sem r CPSExpr) -> Sem r CPSExpr
compileTuple [] k     = k (C.Tuple [])
compileTuple (x:xs) k = compileExpr x \x' -> compileTuple xs \case
    C.Tuple fs -> k (C.Tuple (x':fs))
    e -> error $ "compileTuple did not return a tuple expression: " <> show e

{-
[compileExpr (Tuple fs) k]

[|Tuple [a, b, c]|] k = [|a|] \a' -> [|b|] \b' -> [|c|] \c' -> k (Tuple a' b' c')

-}

compileTailCall :: (Member (State Int) r, HasCallStack) => LCExpr -> CPSExpr -> Sem r CPSExpr
compileTailCall (L.App func arg) k = compileExpr func \f -> compileExpr arg \a -> pure (C.App (C.App f a) k)
compileTailCall e _ = error $ "compileTailCall called on non application: " <> show e


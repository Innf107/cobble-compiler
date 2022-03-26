module Language.Cobble.Codegen.CoreToRacket where

import Language.Cobble.Prelude
import Language.Cobble.Core.Types as F
import Language.Cobble.Racket.Types as R
import Language.Cobble.Types.QualifiedName

compile :: [Decl] -> Sem r [RacketExpr] 
compile = evalState initialState . compile'
    where
        initialState = CompState {
                            varTypes = mempty    
                        }

data CompState = CompState {
        varTypes :: Map QualifiedName Type
    } deriving (Show, Eq, Generic, Data)

insertVarType :: Members '[State CompState] r => QualifiedName -> Type -> Sem r ()
insertVarType x ty = modify (\s@CompState{varTypes} -> s{varTypes = insert x ty varTypes})

lookupVarType :: Members '[State CompState] r => QualifiedName -> Sem r Type
lookupVarType x = gets \CompState{varTypes} -> case lookup x varTypes of
    Just ty -> ty
    _ -> error $ "lookupVarType: core variable '" <> show x <> "' not found. This should have been caught by core lint."

compile' :: Members '[State CompState] r => [Decl] -> Sem r [RacketExpr] 
compile' [] = pure []
compile' (Def x _ty e : ds) = (:) 
    <$> (RDefine x <$> compileExpr e) 
    <*> compile' ds
compile' (DefVariant x args clauses : ds) = compile' ds

compileExpr :: Members '[State CompState] r => Expr -> Sem r RacketExpr
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
compileExpr (Let x ty e1 e2) = do
    insertVarType x ty
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    pure $ RLet [(x, e1')] [e2']
compileExpr (If c th el) = RIf
    <$> compileExpr c
    <*> compileExpr th
    <*> compileExpr el
compileExpr (VariantConstr x i _tyArgs valArgs) = RList . (RIntLit i :) . toList <$> traverse compileExpr valArgs
compileExpr (Case scrut branches) = do
    scrutTy <- lookupVarType scrut
    let actualScrut
            | scrutTy == intTy = RVar scrut
            | otherwise        = RCadr 0 (RVar scrut) -- We match on the constructor tag

    RCase actualScrut
        <$> traverse compileBranch branches
        where
            compileBranch (PInt i, e) = (RIntP [i],) <$> compileExpr e
            compileBranch (PConstr _ [] i, e) = (RIntP [i],) <$> compileExpr e
            compileBranch (PConstr _ args i, e) = 
                (RIntP [i],) 
                . RLet (zipWith (\(x, _ty) i -> (x, RCadr i (RVar scrut))) (toList args) [1..]) -- We start at 1, since the constructor tag is stored at index 0
                . pure 
                <$> do
                    traverse_ (uncurry insertVarType) args
                    compileExpr e
            compileBranch (PWildcard, e) = (RWildcardP,) <$> compileExpr e
compileExpr (Join j _tys vals body e) = do
    body' <- compileExpr body
    RLet [(j, RLambda (map fst $ toList vals) [body'])] . pure <$> compileExpr e
compileExpr (Jump j _tyArgs valArgs _resTy) = RApp (RVar j) <$> traverse compileExpr (toList valArgs)
compileExpr (PrimOp _ _ _) = undefined


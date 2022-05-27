module Cobble.Codegen.CoreToRacket where

import Cobble.Prelude hiding (EQ)
import Cobble.Core.Syntax as F
import Cobble.Racket.Types as R
import Cobble.Syntax.QualifiedName
import Cobble.Interface

import Cobble.Syntax qualified as C

import Cobble.Util.Polysemy.Fresh

import Cobble.Codegen.PrimOp

compile :: Members '[Fresh Text QualifiedName] r =>  Seq Interface -> Seq Decl -> Sem r (Seq RacketExpr) 
compile interfaces = evalState initialState . compile'
    where
        initialState = foldr insertInterface (CompState mempty mempty) interfaces

insertInterface :: Interface -> CompState -> CompState
insertInterface (Interface {interfaceCoreModSig=CoreModSig{coreModVars, coreModDictTyDefs}}) (CompState vts dfns) = 
    CompState (coreModVars <> vts) (fmap (map fst . snd) coreModDictTyDefs <> dfns)


data CompState = CompState {
        varTypes :: Map QualifiedName Type
    ,   dictFieldNames :: Map QualifiedName (Seq QualifiedName)
    } deriving (Show, Eq, Generic, Data)

insertVarType :: Members '[State CompState] r => QualifiedName -> Type -> Sem r ()
insertVarType x ty = modify (\s@CompState{varTypes} -> s{varTypes = insert x ty varTypes})

lookupVarType :: Members '[State CompState] r => QualifiedName -> Sem r Type
lookupVarType x = gets \CompState{varTypes} -> case lookup x varTypes of
    Just ty -> ty
    _ -> error $ "lookupVarType: core variable '" <> show x <> "' not found. This should have been caught by core lint."

insertDictFieldNames :: Members '[State CompState] r => QualifiedName -> Seq QualifiedName -> Sem r ()
insertDictFieldNames x names = modify (\s@CompState{dictFieldNames} -> s{dictFieldNames = insert x names dictFieldNames})

lookupDictFieldNames :: Members '[State CompState] r => QualifiedName -> Sem r (Seq QualifiedName)
lookupDictFieldNames x = gets \CompState{dictFieldNames} -> case lookup x dictFieldNames of
    Just names -> names
    _ -> error $ "lookupDictFieldNames: core dictionary '" <> show x <> "' not found."


compile' :: Members '[Fresh Text QualifiedName, State CompState] r => Seq Decl -> Sem r (Seq RacketExpr)
compile' Empty = pure []
compile' (Def x _ty e :<| ds) = (<|) 
    <$> (RDefine x <$> compileExpr e) 
    <*> compile' ds
compile' (DefVariant x args clauses :<| ds) = compile' ds
compile' (DefDict x args fields :<| ds) = do
    insertDictFieldNames x (map fst fields)
    compile' ds
-- Effect definitions are not actually necessary for compilation, since performing functions are generated separately.
compile' (DefEffect x args fields :<| ds) = do
    compile' ds

compileExpr :: Members '[Fresh Text QualifiedName, State CompState] r => Expr -> Sem r RacketExpr
compileExpr e@Var{} = compileApp e []
compileExpr (App e1 e2) = do
    e2' <- compileExpr e2
    compileApp e1 [e2']

compileExpr (TyApp e _ty) = compileExpr e
compileExpr (Abs x _eff _ty e) = do
    e' <- compileExpr e
    pure (RLambda [x] [e'])
compileExpr (TyAbs _ _ e) = compileExpr e
compileExpr (IntLit n) = pure $ RIntLit n
compileExpr (Let x ty e1 e2) = do
    insertVarType x ty
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    pure $ RLet [(x, e1')] [e2']
compileExpr (If c th el) = RIf
    . (\c' -> REQ [RCadr 0 c', RIntLit 1]) <$> compileExpr c
    <*> compileExpr th
    <*> compileExpr el
compileExpr (VariantConstr x i _tyArgs valArgs) = RList . (RIntLit i <|) <$> traverse compileExpr valArgs
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
                . RLet (mapWithIndex (\i (x, _ty) -> (x, RCadr (i + 1) (RVar scrut))) args) -- We start at 1, since the constructor tag is stored at index 0
                . pure 
                <$> do
                    traverse_ (uncurry insertVarType) args
                    compileExpr e
            compileBranch (PWildcard, e) = (RWildcardP,) <$> compileExpr e
compileExpr (Join j _tys vals body e) = do
    body' <- compileExpr body
    RLet [(j, RLambda (map fst vals) [body'])] . pure <$> compileExpr e
compileExpr (Jump j _tyArgs valArgs _resTy) = RApp (RVar j) <$> traverse compileExpr valArgs

compileExpr (DictConstruct className tyArgs methods) = do
    fieldNames <- lookupDictFieldNames className
    RHash <$> zipWithM (\x expr -> (RSymbol x,) <$> compileExpr expr) fieldNames methods
compileExpr (DictAccess expr _className _tyArgs field) = RHashRef <$> compileExpr expr <*> pure (RSymbol field)
compileExpr (Perform effName op _tyArgs [valArg]) = do
    valArg' <- compileExpr valArg
    pure $ RApp (RBuiltin "perform") [RSymbol effName, RSymbol op, valArg']
compileExpr e@(Perform _ _ _ valArgs) = do
    error $ "Effect operations with multiple arguments NYI: " <> show e
compileExpr e@(Handle expr eff handlers retClause) = do
    expr' <- compileExpr expr
    let effName = getConstrName eff

    handlerHash <- RHash <$> forM handlers \(op, args, expr) -> do
        case args of 
            [(arg, _argTy)] -> do
                expr' <- compileExpr expr
                pure (RSymbol op, RLambda [arg, resumptionVar] [expr'])
            _ -> error $ "Effect operations with multiple arguments NYI: " <> show e

    pure $ RApp (RBuiltin "handler") [RSymbol effName, handlerHash, RLambda [] [expr']]
        where
            getConstrName (TCon name _) = name
            getConstrName (TApp ty _) = getConstrName ty
            getConstrName _ = error $ "CoreToRacket.compileExpr: Not a valid effect type: " <> show eff
compileExpr (Resume expr) = do
    RApp (RVar resumptionVar) . pure <$> compileExpr expr

resumptionVar :: QualifiedName
resumptionVar = internalQName "resumption"


compileApp :: Members '[Fresh Text QualifiedName, State CompState] r => Expr -> Seq RacketExpr -> Sem r RacketExpr
compileApp (App e1 e2) args = do
    e2' <- compileExpr e2
    compileApp e1 (e2' <| args)
compileApp (TyApp e _ty) args = compileApp e args
compileApp (Var x) args = case lookup x primOps of
    Nothing -> pure (foldl' app (RVar x) args)
    Just PrimOpInfo {primOp, primOpType} -> do
        let remainingArgCount = countFunArgs primOpType - length args
        remainingArgs <- replicateM remainingArgCount (freshVar "x")
        pure $ foldr lambda (compilePrimOp primOp (args <> map RVar remainingArgs)) remainingArgs
            where
                lambda x e = RLambda [x] [e]
                countFunArgs (C.TForall _ ty) = countFunArgs ty
                countFunArgs (C.TFun _ _ b) = 1 + countFunArgs b
                countFunArgs _ = 0
compileApp e args = do
    e' <- compileExpr e
    pure (foldl' app e' args)

app :: RacketExpr -> RacketExpr -> RacketExpr
app e1 e2 = RApp e1 [e2]

compilePrimOp :: PrimOp -> Seq RacketExpr -> RacketExpr
compilePrimOp True_ _       = RTrue
compilePrimOp False_ _      = RFalse
compilePrimOp Add [x, y]    = RAdd [x, y]
compilePrimOp Add xs        = error $ "compilePrimOp: wrong arguments for 'add#': " <> show xs
compilePrimOp Sub [x, y]    = RSub [x, y]
compilePrimOp Sub xs        = error $ "compilePrimOp: wrong arguments for 'sub#': " <> show xs
compilePrimOp Mul [x, y]    = RMul [x, y]
compilePrimOp Mul xs        = error $ "compilePrimOp: wrong arguments for 'mul#': " <> show xs
compilePrimOp IntDiv [x, y] = RQuotient [x, y]
compilePrimOp IntDiv xs     = error $ "compilePrimOp: wrong arguments for 'intdiv#': " <> show xs
compilePrimOp Mod [x, y]    = RMod [x, y]
compilePrimOp Mod xs        = error $ "compilePrimOp: wrong arguments for 'mod#': " <> show xs
compilePrimOp LE [x, y]     = RLE [x, y]
compilePrimOp LE xs         = error $ "compilePrimOp: wrong arguments for 'le#': " <> show xs
compilePrimOp EQ [x, y]     = REQ [x, y]
compilePrimOp EQ xs         = error $ "compilePrimOp: wrong arguments for 'eq#': " <> show xs
compilePrimOp Debug [x]     = RDisplayln x
compilePrimOp Debug xs      = error $ "compilePrimOp: wrong arguments for 'debug#': " <> show xs

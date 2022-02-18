module Language.Cobble.Codegen.CobbleToLC where

import Language.Cobble.Prelude hiding (EQ)
import Language.Cobble.Util
import Language.Cobble.Types as C

import Language.Cobble.LC.Types as L

import Language.Cobble.Codegen.PrimOps

import Language.Cobble.Util.Polysemy.Fresh

import Data.Text qualified as T

compile :: forall r. Members '[Fresh Text QualifiedName] r => Map QualifiedName PrimOpInfo -> Module Codegen -> Sem r [LCDef]
compile prims (Module _deps _modname statements) = concat <$> traverse compileStatement statements
    where
        compileStatement :: Statement Codegen -> Sem r [LCDef]
        compileStatement Import {} = pure []
        compileStatement DefStruct {} = pure []
        compileStatement DefVariant {} = pure []
        compileStatement (DefClass _ _li _cname _tvs meths) = pure $ zipWith defMethod [0..] meths
            where 
                defMethod i (mname, _mty) = let dname = todo (internalQName "dict") 
                    in LCDef mname $ L.Lambda dname (Select i (L.Var dname))
        
        compileStatement (DefInstance _classMeths _li cname ty meths) = sequence [
                LCDef (dictName cname ty) . Tuple <$> traverse (compileToLambdaWithout cname) meths
            ]
        compileStatement (Def _ _li decl@(Decl (_, _) fname _ _) _) = sequence [
                LCDef fname <$> compileToLambda decl
            ]

        compileExpr :: Expr Codegen -> Sem r LCExpr
        compileExpr (C.IntLit _li _ i) = pure $ L.IntLit i
        compileExpr (UnitLit _li)      = pure $ Tuple []
        -- If a variable's type needs a constraint, we have to compile that to a function call accepting a parameter
        -- for the given constraint dictionary.
        --
        -- Example:
        -- x :: Num a => a 
        -- let y :: Int = x
        -- ->
        -- x :: Num a -> a
        -- let y :: Int = x d_Num_Int
        compileExpr (C.Var (_, ws) _ n) = pure $ foldl' addConstraint (L.Var n) ws
            where
                addConstraint ex (TWanted (MkConstraint cname t) _) = App ex (L.Var (dictName cname (coercePass t)))

        compileExpr (C.Ascription _ _ e _) = compileExpr e

        compileExpr (C.VariantConstr (_, 0, i) _ n) = pure $ L.Tuple [L.IntLit i]
        compileExpr (C.VariantConstr (_, expectedParams, i) _ n) = do 
            params <- replicateM expectedParams (freshVar "v")
            pure $ foldr L.Lambda (Tuple (L.IntLit i : (map L.Var params))) params
        compileExpr (FCall _ _ (C.VariantConstr (_, expectedParams, i) _ con) as)
            | expectedParams == length as = L.Tuple . (L.IntLit i:) <$> traverse compileExpr (toList as)
            | expectedParams >  length as = do
                remainingParams <- replicateM (expectedParams - length as) (freshVar "v")
                as' <- toList <$> traverse compileExpr as
                pure $ foldr L.Lambda (L.Tuple ([L.IntLit i] <> as' <> (map L.Var remainingParams))) remainingParams 
            | expectedParams <  length as = error $ "LC Codegen: too many arguments in variant construction. Expected: " <> show expectedParams <> ". Recieved: " <> show (length as) <> "."
        
        compileExpr (C.Case _ty li expr cases) = do
            expr' <- compileExpr expr
            branches <- traverse ((\(CaseBranch () _ p e) -> (p,) <$> compileExpr e)) cases
            pure $ compileCases li expr' branches
            


        -- Primops are passed on and only compiled in TopLevelCPSToMCAsm.hs
        compileExpr (FCall _ _l (C.Var (_, _) _ v) ps) 
            | Just p <- lookup v prims = PrimOp (view primOp p) <$> traverse compileExpr (toList ps) 
        
        compileExpr (FCall _ _ funEx pars) = foldl' App <$> (compileExpr funEx) <*> (traverse compileExpr pars)
        
        compileExpr (C.Let () _ decl@(Decl (_, _) name _ _) body) =
            L.Let name <$> compileToLambda decl <*> compileExpr body
        
        compileExpr (StructConstruct _ _ _ fs) = Tuple <$> traverse (compileExpr . snd) fs
        compileExpr (StructAccess (def, _) _ structExp fieldName) = case findIndexOf (structFields . folded) (\(x,_) -> x == fieldName) def of
            Nothing -> error "LC Codegen Panic: structAccess: field not found too late"
            Just i -> Select i <$> compileExpr structExp
        
        compileExpr (C.If _ _ c th el)  = L.If <$> compileExpr c <*> compileExpr th <*> compileExpr el
        
        compileExpr (C.Lambda _ _ x e) = L.Lambda x <$> compileExpr e

        compileToLambda :: Decl Codegen -> Sem r LCExpr
        compileToLambda (Decl (_ty, gs) _ xs e) = foldr addGiven <$> (foldr L.Lambda <$> compileExpr e <*> pure (map fst xs)) <*> pure gs

        compileToLambdaWithout :: QualifiedName -> Decl Codegen -> Sem r LCExpr
        compileToLambdaWithout cname (Decl (_ty, gs) _ xs e) = foldr addGiven <$> (foldr L.Lambda <$> compileExpr e <*> pure (map fst xs)) <*> pure gs'
            where
                gs' = filter (\(TGiven (MkConstraint c _) _) -> c /= cname) gs

        addGiven (TGiven (MkConstraint cname t) _) ex = L.Lambda (dictName cname (coercePass t)) ex

compileCases :: LexInfo -> LCExpr -> [(Pattern Codegen, LCExpr)] -> LCExpr
compileCases li e cases = foldr combineToIf (nonExhaustiveBranch li) cases
    where
        combineToIf :: (Pattern Codegen, LCExpr) -> LCExpr -> LCExpr
        combineToIf (p, br) rest = case caseToPredicate e p of
            (Nothing, bindings) -> bindings br
            (Just pred, bindings) -> L.If pred (bindings br) rest

nonExhaustiveBranch :: LexInfo -> LCExpr
nonExhaustiveBranch li = Fail $ "Non-exhaustive patterns in case at " <> show li

-- Returns a compiled predicate (if applicable) and a transformation that binds
-- matched variables
caseToPredicate :: LCExpr -> Pattern Codegen -> (Maybe LCExpr, LCExpr -> LCExpr)
caseToPredicate e (IntP () n) = (Just (PrimOp EQ [e, L.IntLit n]), id)
caseToPredicate e (VarP _ x) = (Nothing, L.Let x e)
caseToPredicate e (ConstrP (_, i) constr ps) = (Just pred, \e -> foldr ($) e subBindings)
    where
        (subPreds, subBindings) = unzip $ zipWith (\j p -> caseToPredicate (Select j e) p) [1..] ps 
        pred = PrimOp EQ [Select 0 e, L.IntLit i] 
                `and'` 
                foldr and' (PrimOp True_ [Tuple []]) (catMaybes subPreds)

and' :: LCExpr -> LCExpr -> LCExpr
and' x y = L.If x y (PrimOp False_ [Tuple []])

dictName :: QualifiedName -> Type Codegen -> QualifiedName
dictName (UnsafeQualifiedName original i li) ty = UnsafeQualifiedName ("d_" <> original <> showTypeName ty) i li
    
showTypeName :: Type Codegen -> Text
showTypeName (TCon n _) = renderDebug n
showTypeName (TApp t1 t2) = showTypeName t1 <> "_" <> showTypeName t2
showTypeName (TFun t1 t2) = "fun_" <> showTypeName t1 <> "_" <> showTypeName t2
showTypeName (TVar (MkTVar n _)) = "v" <> renderDebug n
showTypeName (TSkol (MkTVar n _)) = "s" <> renderDebug n
showTypeName (TForall ps t) = "forall-" <> T.intercalate "-" (map (\(MkTVar n _) -> renderDebug n) ps)  <> "_" <> showTypeName t
showTypeName (TConstraint (MkConstraint cname ct) t) = "constr-" <> renderDebug cname <> "-" <> showTypeName ct <> "_" <> showTypeName t

collapseDefs :: [LCDef] -> LCExpr
collapseDefs = foldr makeLet (L.IntLit 0) 
    where
        makeLet (LCDef n (L.Lambda x e)) r
            | length [() | L.App (L.Var v) _ <- universeBi e, v == n] > 0 = L.LetRec n x e r
        makeLet (LCDef n e) r = L.Let n e r


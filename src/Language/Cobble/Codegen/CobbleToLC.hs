module Language.Cobble.Codegen.CobbleToLC where

import Language.Cobble.Prelude
import Language.Cobble.Util
import Language.Cobble.Types as C

import Language.Cobble.LC.Types as L

import Language.Cobble.Codegen.PrimOps

import Data.Text qualified as T

compile :: Map QualifiedName PrimOpInfo -> Module Codegen -> [LCDef]
compile prims (Module _deps _modname statements) = concatMap compileStatement statements
    where
        compileStatement :: Statement Codegen -> [LCDef]
        compileStatement Import {} = []
        compileStatement DefStruct {} = []
        compileStatement DefVariant {} = []
        compileStatement (DefClass (Ext _) _li _cname _tvs meths) = zipWith defMethod [0..] meths
            where 
                defMethod i (mname, _mty) = let dname = todo (internalQName "dict") 
                    in LCDef mname $ Lambda dname (Select i (L.Var dname))
        
        compileStatement (DefInstance (Ext _classMeths) _li cname ty meths) = [
                LCDef (dictName cname ty) $ Tuple (map (compileToLambdaWithout cname) meths)
            ]
        compileStatement (Def IgnoreExt _li decl@(Decl (Ext2_1 _ _) fname (Ext _) _) _) = [
                LCDef fname $ compileToLambda decl
            ]

        compileExpr :: Expr Codegen -> LCExpr
        compileExpr (C.IntLit _li _ i) = L.IntLit i
        compileExpr (UnitLit _li)      = Tuple []
        -- If a variable's type needs a constraint, we have to compile that to a function call accepting a parameter
        -- for the given constraint dictionary.
        --
        -- Example:
        -- x :: Num a => a 
        -- let y :: Int = x
        -- ->
        -- x :: Num a -> a
        -- let y :: Int = x d_Num_Int
        compileExpr (C.Var (Ext2_1 _ ws) _ n) = foldl' addConstraint (L.Var n) ws
            where
                addConstraint ex (TWanted (MkConstraint cname t) _) = App ex (L.Var (dictName cname (coercePass t)))

        compileExpr (C.VariantConstr (Ext (_, 0, i)) _ n) = Variant (n, i) []
        compileExpr (C.VariantConstr (Ext (_, _expectedParams, _i)) _ _n) = error $ "LC Codegen: Variant construction for partially applied constructor without args NYI"  
        
        compileExpr (FCall _ _ (C.VariantConstr (Ext (_, expectedParams, i)) _ con) as)
            | expectedParams == length as = L.Variant (con, i) (map compileExpr $ toList as)
            | expectedParams >  length as = error "LC Codegen: Variant construction for partially applied constructor NYI"
            | expectedParams <  length as = error $ "LC Codegen: too many arguments in variant construction. Expected: " <> show expectedParams <> ". Recieved: " <> show (length as) <> "."
        
        -- Primops are passed on and only compiled in TopLevelCPSToMCAsm.hs
        compileExpr (FCall (Ext _) _l (C.Var (Ext2_1 _ _) _ v) ps) 
            | Just p <- lookup v prims = PrimOp (view primOp p) (map compileExpr $ toList ps) 
        
        compileExpr (FCall (Ext _) _ funEx pars) = foldl' App (compileExpr funEx) (fmap compileExpr pars)
        
        compileExpr (C.Let IgnoreExt _ decl@(Decl (Ext2_1 _ _) name (Ext _) _) body) =
            L.Let name (compileToLambda decl) (compileExpr body)
        
        compileExpr (StructConstruct (Ext _) _ _ fs) = Tuple (map (compileExpr . snd) fs)
        compileExpr (StructAccess (Ext (def, _)) _ structExp fieldName) = case findIndexOf (structFields . folded) (\(x,_) -> x == fieldName) def of
            Nothing -> error "LC Codegen Panic: structAccess: field not found too late"
            Just i -> Select i (compileExpr structExp)
        
        compileExpr (C.If _ _ c th el)  = L.If (compileExpr c) (compileExpr th) (compileExpr el)

        compileToLambda :: Decl Codegen -> LCExpr
        compileToLambda (Decl (Ext2_1 _ty gs) _ (Ext xs) e) = foldr addGiven (foldr Lambda (compileExpr e) (map fst xs)) gs

        compileToLambdaWithout :: QualifiedName -> Decl Codegen -> LCExpr
        compileToLambdaWithout cname (Decl (Ext2_1 _ty gs) _ (Ext xs) e) = foldr addGiven (foldr Lambda (compileExpr e) (map fst xs)) gs'
            where
                gs' = filter (\(TGiven (MkConstraint c _) _) -> c /= cname) gs

        addGiven (TGiven (MkConstraint cname t) _) ex = Lambda (dictName cname (coercePass t)) ex

dictName :: QualifiedName -> Type Codegen -> QualifiedName
dictName (ReallyUnsafeQualifiedName original renamed li) ty = unsafeQualifiedName ("d_" <> original <> showTypeName ty) ("d_" <> renamed <> showTypeName ty) li
    
showTypeName :: Type Codegen -> Text
showTypeName (TCon n _) = renamed n
showTypeName (TApp t1 t2) = showTypeName t1 <> "_" <> showTypeName t2
showTypeName (TVar (MkTVar n _)) = "v" <> renamed n
showTypeName (TSkol (MkTVar n _)) = "s" <> renamed n
showTypeName (TForall ps t) = "forall-" <> T.intercalate "-" (map (\(MkTVar n _) -> renamed n) ps)  <> "_" <> showTypeName t
showTypeName (TConstraint (MkConstraint cname ct) t) = "constr-" <> renamed cname <> "-" <> showTypeName ct <> "_" <> showTypeName t

collapseDefs :: [LCDef] -> LCExpr
collapseDefs = foldr makeLet (L.IntLit 0) 
    where
        makeLet (LCDef n (Lambda x e)) r
            | length [() | L.App (L.Var v) _ <- universeBi e, v == n] > 0 = L.LetRec n x e r
        makeLet (LCDef n e) r = L.Let n e r


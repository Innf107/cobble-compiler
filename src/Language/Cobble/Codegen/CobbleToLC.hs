module Language.Cobble.Codegen.CobbleToLC where

import Language.Cobble.Prelude
import Language.Cobble.Util
import Language.Cobble.Types as C

import Language.Cobble.LC.Types as L

import Language.Cobble.Codegen.PrimOps

compile :: Map QualifiedName PrimOpInfo -> Module Codegen -> LCExpr
compile prims (Module _deps _modname statements) = foldr makeLet (L.IntLit 0) 
    $ concatMap compileStatement statements
    where
        makeLet (LCDef n e) r
            | length [() | L.App (L.Var v) _ <- universeBi e, v == n] > 0 = L.LetRec n e r
            | otherwise = L.Let n e r

        compileStatement :: Statement Codegen -> [LCDef]
        compileStatement Import {} = []
        compileStatement DefStruct {} = []
        compileStatement (StatementX x _) = absurd x
        compileStatement (Def IgnoreExt _li (Decl (Ext _) fname (Ext params) body) _) = [
                LCDef fname $ foldr (Lambda . fst) (compileExpr body) params
            ]

        compileExpr :: Expr Codegen -> LCExpr
        compileExpr (C.IntLit _ _ i) = L.IntLit i
        compileExpr (UnitLit _)      = Tuple []
        compileExpr (C.Var _ _ n)    = L.Var n
        compileExpr (FCall (Ext _) l (C.Var (Ext _) _ v) ps) 
            | Just p <- lookup v prims = PrimOp (view primOp p) (map compileExpr $ toList ps) 
        compileExpr (FCall (Ext _) _ funEx pars) = foldl' App (compileExpr funEx) (fmap compileExpr pars)
        compileExpr (C.Let IgnoreExt _ (Decl (Ext _) name (Ext params) letEx) body) =
            L.Let name (foldr (Lambda . fst) (compileExpr letEx) params) (compileExpr body)
        compileExpr (StructConstruct (Ext _) _ _ fs) = Tuple (map (compileExpr . snd) fs)
        compileExpr (StructAccess (Ext (def, _)) _ structExp fieldName) = case findIndexOf (structFields . folded) (\(x,_) -> unqualifyName x == fieldName) def of
            Nothing -> error "LC Codegen Panic: structAccess: field not found too late"
            Just i -> Select i (compileExpr structExp)
        compileExpr (C.If _ _ c th el)  = L.If (compileExpr c) (compileExpr th) (compileExpr el)
        compileExpr (ExprX x _) = absurd x

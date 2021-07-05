module Language.Cobble.Codegen.CobbleToLC where

import Language.Cobble.Prelude
import Language.Cobble.Util
import Language.Cobble.Types as C

import Language.Cobble.LC.Types as L

compile :: Module Codegen -> [LCDef]
compile (Module _deps _modname statements) = concatMap compileStatement statements

compileStatement :: Statement Codegen -> [LCDef]
compileStatement (Import _ _ _) = []
compileStatement (DefStruct _ _ _ _) = []
compileStatement (StatementX x _) = absurd x
compileStatement (Def IgnoreExt _li (Decl (Ext _) fname (Ext params) body) _) = [ --TODO: Use fix instead of lambda
        LCDef fname $ foldr Lambda (compileExpr body) (map fst params)
    ]

compileExpr :: Expr Codegen -> LCExpr
compileExpr (C.IntLit _ _ i) = L.IntLit i
compileExpr (UnitLit _)      = Tuple []
compileExpr (C.Var _ _ n)    = L.Var n
compileExpr (FCall (Ext _) _ funEx pars) = foldl' App (compileExpr funEx) (fmap compileExpr pars)
compileExpr (Let IgnoreExt _ (Decl (Ext _) name (Ext params) letEx) body) =
    App (Lambda name (compileExpr body)) (foldr Lambda (compileExpr letEx) (map fst params))
compileExpr (StructConstruct (Ext _) _ _ fs) = Tuple (map (compileExpr . snd) fs)
compileExpr (StructAccess (Ext (def, _)) _ structExp fieldName) = case findIndexOf (structFields . folded) (\(x,_) -> unqualifyName x == fieldName) def of
    Nothing -> error "LC Codegen Panic: structAccess: field not found too late"
    Just i -> Select i (compileExpr structExp)
compileExpr (If _ _ _ _ _) = undefined
compileExpr (ExprX x _) = absurd x

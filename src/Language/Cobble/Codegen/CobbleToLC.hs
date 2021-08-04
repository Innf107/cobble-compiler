module Language.Cobble.Codegen.CobbleToLC where

import Language.Cobble.Prelude
import Language.Cobble.Util
import Language.Cobble.Types as C
    ( unqualifyName,
      absurd,
      Decl(Decl),
      Expr(..),
      Ext(Ext),
      IgnoreExt(IgnoreExt),
      Module(Module),
      Pass(Codegen),
      Statement(..),
      structFields )

import Language.Cobble.LC.Types as L

compile :: Module Codegen -> [LCDef]
compile (Module _deps _modname statements) = concatMap compileStatement statements

compileStatement :: Statement Codegen -> [LCDef]
compileStatement Import {} = []
compileStatement DefStruct {} = []
compileStatement (StatementX x _) = absurd x
compileStatement (Def IgnoreExt _li (Decl (Ext _) fname (Ext params) body) _) = [ --TODO: Use fix instead of lambda
        LCDef fname $ foldr (Lambda . fst) (compileExpr body) params
    ]

compileExpr :: Expr Codegen -> LCExpr
compileExpr (C.IntLit _ _ i) = L.IntLit i
compileExpr (UnitLit _)      = Tuple []
compileExpr (C.Var _ _ n)    = L.Var n
compileExpr (FCall (Ext _) _ funEx pars) = foldl' App (compileExpr funEx) (fmap compileExpr pars)
compileExpr (Let IgnoreExt _ (Decl (Ext _) name (Ext params) letEx) body) =
    App (Lambda name (compileExpr body)) (foldr (Lambda . fst) (compileExpr letEx) params)
compileExpr (StructConstruct (Ext _) _ _ fs) = Tuple (map (compileExpr . snd) fs)
compileExpr (StructAccess (Ext (def, _)) _ structExp fieldName) = case findIndexOf (structFields . folded) (\(x,_) -> unqualifyName x == fieldName) def of
    Nothing -> error "LC Codegen Panic: structAccess: field not found too late"
    Just i -> Select i (compileExpr structExp)
compileExpr If {} = undefined
compileExpr (ExprX x _) = absurd x

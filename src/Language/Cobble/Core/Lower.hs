module Language.Cobble.Core.Lower where

import Language.Cobble.Prelude

import qualified Language.Cobble.Types as C

import qualified Language.Cobble.Core.Types as F

type CExpr = C.Expr C.Codegen

lowerExpr :: CExpr -> Sem r F.Expr
lowerExpr (C.App ty _ e1 e2) = F.App -- TODO: type applications?
                               <$> lowerExpr e1
                               <*> lowerExpr e2
lowerExpr (C.IntLit () _ n) = pure $ F.IntLit n
lowerExpr (C.If ty _ c th el) = F.If 
                                <$> lowerExpr c
                                <*> lowerExpr th
                                <*> lowerExpr el
lowerExpr (C.Let _ li (C.Decl (ty, _) f xs e1) e2) = F.Let f 
                                <$> lowerType ty
                                <*> lowerExpr (foldr (\(x, t) r -> C.Lambda t li x r) e1 xs) 
                                <*> lowerExpr e2
lowerExpr (C.Var _ _ x) = pure $ F.Var x
lowerExpr (C.Ascription _ _ e ty) = undefined
lowerExpr _ = undefined

lowerType :: C.Type -> Sem r F.Type
lowerType = undefined


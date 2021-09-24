module Language.Cobble.PostProcess where

import Language.Cobble.Prelude
import Language.Cobble.Types

type NextPass = Codegen

-- boilerplate :/
postProcess :: Module PostProcess -> Module NextPass
postProcess (Module (Ext deps) n sts) = Module (Ext deps) n (map postProcessStatement sts)

-- more boilerplate :/
postProcessStatement :: Statement PostProcess -> Statement NextPass
postProcessStatement (Def IgnoreExt li (Decl (Ext ty') f (Ext xs) e) ty) = 
    Def IgnoreExt li (Decl (Ext (coercePass ty')) f (Ext (map (second coercePass) xs)) (postProcessExpr e)) (coercePass ty)-- 
postProcessStatement (Import IgnoreExt li n) = Import IgnoreExt li n
postProcessStatement (DefStruct (Ext k) li sname ps fields) = 
    DefStruct (Ext k) li sname (coercePass ps) (map (second coercePass) fields)
postProcessStatement (DefVariant (Ext k) li vname ps constrs) = 
    DefVariant (Ext k) li vname (coercePass ps) (map (second coercePass) constrs)
postProcessStatement (StatementX x _) = absurd x

postProcessExpr :: Expr PostProcess -> Expr NextPass
postProcessExpr = \case 
    -- The interesting case
    StructAccess (Ext (possibleStructs, structTy, ty)) li sexpr field -> 
        let sname = getStructName structTy
            sdef = fromMaybe (error $ "postProcessExpr: struct name was not in possibleStructs: " <> show sname) (lookup sname possibleStructs) 
        in
        StructAccess(Ext (coercePass sdef, (coercePass ty))) li (postProcessExpr sexpr) field

    -- just coercePass wrappers and recursion (would love to factor this out with uniplate...)
    FCall (Ext ty) li f as -> FCall (Ext (coercePass ty)) li (postProcessExpr f) (fmap postProcessExpr as) 
    IntLit IgnoreExt li n -> IntLit IgnoreExt li n
    UnitLit li -> UnitLit li
    If IgnoreExt li cond th el -> If IgnoreExt li (postProcessExpr cond) (postProcessExpr th) (postProcessExpr el) 
    Let IgnoreExt li (Decl (Ext ty) f (Ext xs) e) b -> 
        Let IgnoreExt li (Decl (Ext (coercePass ty)) f (Ext (map (second coercePass) xs)) (postProcessExpr e)) (postProcessExpr b)
    Var (Ext ty) li n -> Var (Ext (coercePass ty)) li n
    VariantConstr (Ext ty) li n -> VariantConstr (Ext (coercePass ty)) li n
    StructConstruct (Ext (sd, ty)) li sname fexprs -> 
        StructConstruct (Ext (coercePass sd, coercePass ty)) li sname (map (second postProcessExpr) fexprs)
    ExprX x _ -> absurd x
    where
        getStructName :: Type PostProcess -> QualifiedName
        getStructName (TCon name _) = name
        getStructName (TApp t1 _) = getStructName t1
        getStructName (TVar v) = error $ "postProcessExpr: Type checker inferred type variable for StructAccess expression: " <> show v
        getStructName (TSkol v) = error $ "postProcessExpr: Type checker inferred skolem variable for StructAccess expression: " <> show v
        getStructName (TForall _ t) = getStructName t 
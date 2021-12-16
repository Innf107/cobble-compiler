module Language.Cobble.PostProcess where

import Language.Cobble.Prelude
import Language.Cobble.Types

type NextPass = Codegen

-- boilerplate :/
postProcess :: Module PostProcess -> Module NextPass
postProcess (Module deps n sts) = Module deps n (map postProcessStatement sts)

-- more boilerplate :/
postProcessStatement :: Statement PostProcess -> Statement NextPass
postProcessStatement (Def IgnoreExt li decl ty) = 
    Def IgnoreExt li (postProcessDecl decl) (coercePass ty)-- 
postProcessStatement (Import IgnoreExt li n) = Import IgnoreExt li n
postProcessStatement (DefStruct k li sname ps fields) = 
    DefStruct k li sname (coercePass ps) (map (second coercePass) fields)
postProcessStatement (DefVariant k li vname ps constrs) = 
    DefVariant k li vname (coercePass ps) (map (\(n, ts, x) -> (n, coercePass ts, coercePass x)) constrs)
postProcessStatement (DefClass k li sname ps meths) =
    DefClass k li sname (coercePass ps) (map (second coercePass) meths)
postProcessStatement (DefInstance (defs, classPs) li cname ty decls) =
    DefInstance (map (second coercePass) defs, coercePass classPs) li cname (coercePass ty) (map postProcessDecl decls)

postProcessDecl :: Decl PostProcess -> Decl NextPass
postProcessDecl (Decl (Ext2_1 ty' gs) f xs e) = Decl (Ext2_1 (coercePass ty') gs) f (map (second coercePass) xs) (postProcessExpr e)

postProcessExpr :: Expr PostProcess -> Expr NextPass
postProcessExpr = \case 
    -- The interesting case
    StructAccess (possibleStructs, structTy, ty) li sexpr field -> 
        let sname = getStructName structTy
            sdef = fromMaybe (error $ "postProcessExpr: struct name was not in possibleStructs: " <> show sname) (lookup sname possibleStructs) 
        in
        StructAccess (coercePass sdef, (coercePass ty)) li (postProcessExpr sexpr) field

    -- just coercePass wrappers and recursion (would love to factor this out with uniplate...)
    FCall ty li f as -> FCall (coercePass ty) li (postProcessExpr f) (fmap postProcessExpr as) 
    IntLit IgnoreExt li n -> IntLit IgnoreExt li n
    UnitLit li -> UnitLit li
    If IgnoreExt li cond th el -> If IgnoreExt li (postProcessExpr cond) (postProcessExpr th) (postProcessExpr el) 
    Let IgnoreExt li decl b -> 
        Let IgnoreExt li (postProcessDecl decl) (postProcessExpr b)
    Var (Ext2_1 ty ws) li n -> Var (Ext2_1 (coercePass ty) ws) li n
    VariantConstr (ty, e, i) li n -> VariantConstr (coercePass ty, e, i) li n
    Case t li e cases -> Case (coercePass t) li (postProcessExpr e) (map postProcessCaseBranch cases)
    StructConstruct (sd, ty) li sname fexprs -> 
        StructConstruct (coercePass sd, coercePass ty) li sname (map (second postProcessExpr) fexprs)
    where
        getStructName :: Type PostProcess -> QualifiedName
        getStructName (TCon name _) = name
        getStructName (TApp t1 _) = getStructName t1
        getStructName (TVar v) = error $ "postProcessExpr: Type checker inferred type variable for StructAccess expression: " <> show v
        getStructName (TSkol v) = error $ "postProcessExpr: Type checker inferred skolem variable for StructAccess expression: " <> show v
        getStructName (TForall _ t) = getStructName t 
        getStructName (TConstraint _ t) = getStructName t

-- So. Much. Boilerplate.
postProcessCaseBranch :: CaseBranch PostProcess -> CaseBranch NextPass
postProcessCaseBranch (CaseBranch IgnoreExt li p e) = CaseBranch IgnoreExt li (postProcessPattern p) (postProcessExpr e)

postProcessPattern :: Pattern PostProcess -> Pattern NextPass
postProcessPattern (IntP t n) = IntP (coercePass t) n
postProcessPattern (VarP t x) = VarP (coercePass t) x
postProcessPattern (ConstrP t constr ps) = ConstrP (coercePass t) constr (map postProcessPattern ps)


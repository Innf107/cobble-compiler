{-# OPTIONS_GHC -Wno-orphans #-}
module Cobble.Types  
    (
      module Export
    , NameSpace
    , HasType (..)
    )
    where

import Cobble.Types.Lens as Export
import Cobble.Types.AST as Export
import Cobble.Types.AST.SolveModules as Export
import Cobble.Types.AST.QualifyNames as Export
import Cobble.Types.AST.SemAnalysis as Export
import Cobble.Types.AST.Typecheck as Export
import Cobble.Types.AST.Codegen as Export
import Cobble.Types.Instances as Export
import Cobble.Types.QualifiedName as Export
import Cobble.Types.LexInfo as Export

import Cobble.Prelude

import Cobble.Util.Prettyprinter as P

type NameSpace = Text


class HasType t where
    getType :: t -> Type
    setType :: Type -> t -> t


instance HasType (Expr 'Codegen) where
    getType = \case
        App t _ _ _                         -> t
        Var (t, _) _ _                      -> t
        VariantConstr (t,_,_) _ _           -> t
        Case t _ _ _                        -> t
        IntLit _ _ _                        -> intT
        If _ _ _ th _                       -> getType th
        Let _ _ _ b                         -> getType b
        Lambda (t, _, _) _ _ _              -> t
        Handle _ _ e _                      -> getType e
        TyAbs _ tv e                        -> TForall [tv] (getType e)
        TyApp _ targ e                      -> getType e -- TODO: Should apply arg type
        DictAbs _ _ c e                     -> TConstraint c (getType e)
        DictVarApp _ e _                    -> getType e -- TODO: Should apply dictionary
        DictApp _ e _                       -> getType e -- TODO: Should apply dictionary
    setType t = \case
        App _ x y z                         -> App t x y z
        Var (_, x) y z                      -> Var (t, x) y z
        VariantConstr (_,x,y) z w           -> VariantConstr (t, x, y) z w
        Case _ x y z                        -> Case t x y z
        i@IntLit{}                          -> i
        If x y z th el                      -> If x y z (setType t th) (setType t el)
        Let x y z b                         -> Let x y z (setType t b)
        Lambda (_, x, a) y z w              -> Lambda (t, x, a) y z w
        Handle x y e z                      -> Handle x y (setType t e) z
        TyAbs li ty e                       -> TyAbs li ty (setType t e)
        TyApp li targ e                     -> TyApp li targ (setType t e)
        DictAbs li x c e                    -> DictAbs li x c (setType t e)
        DictVarApp li e arg                 -> DictVarApp li (setType t e) arg
        DictApp li e arg                    -> DictVarApp li (setType t e) arg

instance HasType (Decl Codegen) where
    getType (Decl (t, _) _ _ _)   = t
    setType t (Decl (_, x) y z w) = Decl (t, x) y z w

instance HasType (Pattern Codegen) where
    getType (IntP () n)              = intT
    getType (VarP ty _)              = ty
    getType (ConstrP (ty, _, _) _ _) = ty
    getType (WildcardP ty)           = ty
    getType (OrP ty _)               = ty

    setType _ i@IntP{}                = i
    setType t (VarP _ x)              = VarP t x
    setType t (ConstrP (_, x, w) y z) = ConstrP (t, x, w) y z
    setType t (WildcardP _)           = WildcardP t
    setType t (OrP _ pats)            = OrP t pats

instance HasType Type where
    getType     = id
    setType t _ = t

ppQName :: QualifiedName -> Doc ann
ppQName = pretty . renderDebug

line_ :: Doc ann
line_ = P.line

indent_ :: Doc ann -> Doc ann
indent_ = indent 4

instance Pretty (Module SemAnalysis) where
    pretty (Module _ name statements) = "module" <+> pretty name <> ";" <> line_
                                    <> vsep (map ((line_ <>) . pretty) statements)

instance Pretty (Statement SemAnalysis) where
    pretty (Def fixity _ d@(Decl _ f xs e) ty) = 
            prettyFixity fixity <> ppQName f <+> "::" <+> pretty ty <> line_
        <>  prettyDecl d
        where
            prettyFixity Nothing = ""
            prettyFixity (Just (LeftFix f))  = "infixl" <+> pretty f <> line_
            prettyFixity (Just (RightFix f)) = "infixr" <+> pretty f <> line_

    pretty (DefClass k _ cname tvs meths) = 
        "class (" <> ppQName cname <+> "::" <+> pretty k <> ")" <+> encloseSep "" "" " " (map pretty tvs) <+> "{" <> line_
        <> indent_ (
            vsep (map (\(x, ty) -> ppQName x <> " :: " <> pretty ty) meths)
        ) <> line_ <> "}"
    pretty (DefInstance (k, defMeths, defTVs, isImported) _ cname ty decls) =
        "{-" <> line_ <> indent_ (
           "[is imported]:" <+> pretty isImported <> line_
        <> "[Definition]: class" <+> ppQName cname <+> encloseSep "" "" " " (map pretty defTVs) <+> "{" <> line_
        <> indent_ (
            vsep (map (\(x, ty) -> ppQName x <> " :: " <> pretty ty) defMeths)
        ) <> line_ <> "}"
        ) <> line_ <> "-}" <> line_
        <> "instance (" <> ppQName cname <+> "::" <+> pretty k <> ")" <+> pretty ty <+> "{" <> line_
        <> indent_ (
            vsep (map prettyDecl decls)
        ) <> line_ <> "}"
    pretty (DefVariant k _ vname tvs constrs) =
        "variant (" <> ppQName vname <+> "::" <+> pretty k <> ")" <+> encloseSep "" "" " " (map pretty tvs) <+> "{" <> line_ 
        <> indent_ (
            vsep (map (\(cname, ctys, (i, j)) -> 
                ppQName cname <> "[" <> pretty i <> ", " <> pretty j <> "]" 
                <+> encloseSep "" "" " " (map pretty ctys)) constrs)
        ) <> "}"
    pretty (DefEffect k _ effName tvs ops) =
        "effect (" <> ppQName effName <+> "::" <+> pretty k <> ")" <+> encloseSep "" "" " " (map pretty tvs) <+> "{" <> line_
        <> indent_ (
            vsep (map (\(x, ty) -> ppQName x <> " :: " <> pretty ty) ops)
        ) <> line_ <> "}"


prettyDecl :: Decl SemAnalysis -> Doc ann
prettyDecl (Decl _ f xs e) = ppQName f <+> encloseSep "" "" " " (map ppQName xs) <+> "=" <> line_ <> indent_ (pretty e)


instance Pretty TVar where
    pretty (MkTVar var k) = "(" <> ppQName var <+> "::" <+> pretty k <> ")"

instance Pretty (Expr SemAnalysis) where
    pretty _ = "{TODO: Prettyprint expresions}"

instance Pretty Type where
    pretty = pretty . ppType

instance Pretty Kind where
    pretty KStar = "*"
    pretty KConstraint = "Constraint"
    pretty KEffect = "Effect"
    pretty (KRow k) = "KRow (" <> pretty k <> ")" 
    pretty (KFun k1 k2) = "(" <> pretty k1 <> ") -> " <> pretty k2

instance Pretty (Module Typecheck) where
    pretty m = pretty (coercePass @(Module Typecheck) @(Module SemAnalysis) m)


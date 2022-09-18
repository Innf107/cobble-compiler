{-# OPTIONS_GHC -Wno-orphans #-}
module Cobble.Syntax  
    (
      module Export
    , NameSpace
    , HasType (..)
    )
    where

import Cobble.Syntax.Lens as Export
import Cobble.Syntax.AST as Export
import Cobble.Syntax.AST.SolveModules as Export
import Cobble.Syntax.AST.QualifyNames as Export
import Cobble.Syntax.AST.SemAnalysis as Export
import Cobble.Syntax.AST.Typecheck as Export
import Cobble.Syntax.AST.Codegen as Export
import Cobble.Syntax.Instances as Export
import Cobble.Syntax.QualifiedName as Export
import Cobble.Syntax.LexInfo as Export


import Cobble.Prelude
import Cobble.Config (Config(..), getConfig)

import Cobble.Util.Prettyprinter as P

type NameSpace = Text


class HasType t where
    getType :: t -> Type
    setType :: Type -> t -> t


instance HasType (Expr 'Codegen) where
    getType = \case
        App t _ _ _                         -> t
        Var t _ _                           -> t
        VariantConstr (t,_,_) _ _           -> t
        Case t _ _ _                        -> t
        IntLit _ _ _                        -> intT
        If _ _ _ th _                       -> getType th
        Let _ _ _ b                         -> getType b
        Lambda (t, _, _) _ _ _              -> t
        Handle (t, _) _ _ _ _               -> t
        Resume t _ _                        -> t
        TyAbs _ tv e                        -> TForall tv (getType e)
        TyApp _ targ e                      -> getType e -- TODO: Should apply arg type
        DictAbs _ _ c e                     -> TConstraint c (getType e)
        DictVarApp _ e _                    -> getType e -- TODO: Should apply dictionary
        DictApp _ e _                       -> getType e -- TODO: Should apply dictionary
    setType t = \case
        App _ x y z                         -> App t x y z
        Var _ y z                           -> Var t y z
        VariantConstr (_,x,y) z w           -> VariantConstr (t, x, y) z w
        Case _ x y z                        -> Case t x y z
        i@IntLit{}                          -> i
        If x y z th el                      -> If x y z (setType t th) (setType t el)
        Let x y z b                         -> Let x y z (setType t b)
        Lambda (_, x, a) y z w              -> Lambda (t, x, a) y z w
        Handle (_, eff) y e z r             -> Handle (t, eff) y e z r
        Resume _ li e                       -> Resume t li e
        TyAbs li ty e                       -> TyAbs li ty (setType t e)
        TyApp li targ e                     -> TyApp li targ (setType t e)
        DictAbs li x c e                    -> DictAbs li x c (setType t e)
        DictVarApp li e arg                 -> DictVarApp li (setType t e) arg
        DictApp li e arg                    -> DictVarApp li (setType t e) arg

instance HasType (Decl Codegen) where
    getType (Decl t _ _ _)   = t
    setType t (Decl _ y z w) = Decl t y z w

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

ppQName :: QualifiedName -> Doc ann
ppQName = pretty . renderDebug

line_ :: Doc ann
line_ = P.line

indent_ :: Doc ann -> Doc ann
indent_ = indent 4

instance Pretty (Module SemAnalysis) where
    prettyPrec _ (Module _ name statements) = "module" <+> pretty name <> ";" <> line_
                                    <> vsep (map ((line_ <>) . pretty) statements)

-- TODO: Actually use the precedence here
instance Pretty (Statement SemAnalysis) where
    prettyPrec _ (Def fixity _ d@(Decl _ f xs e) ty) = 
            prettyFixity fixity <> ppQName f <+> "::" <+> pretty ty <> line_
        <>  prettyDecl d
        where
            prettyFixity Nothing = ""
            prettyFixity (Just (LeftFix f))  = "infixl" <+> pretty f <> line_
            prettyFixity (Just (RightFix f)) = "infixr" <+> pretty f <> line_

    prettyPrec _ (DefClass k _ cname tvs meths) = 
        "class (" <> ppQName cname <+> "::" <+> pretty k <> ")" <+> encloseSep "" "" " " (map pretty tvs) <+> "{" <> line_
        <> indent_ (
            vsep (map (\(x, ty) -> ppQName x <> " :: " <> pretty ty) meths)
        ) <> line_ <> "}"
    prettyPrec _ (DefInstance (k, defMeths, defTVs, isImported) _ cname ty decls) =
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
    prettyPrec _ (DefVariant k _ vname tvs constrs) =
        "variant (" <> ppQName vname <+> "::" <+> pretty k <> ")" <+> encloseSep "" "" " " (map pretty tvs) <+> "{" <> line_ 
        <> indent_ (
            vsep (map (\(cname, ctys, (i, j)) -> 
                ppQName cname <> "[" <> pretty i <> ", " <> pretty j <> "]" 
                <+> encloseSep "" "" " " (map pretty ctys)) constrs)
        ) <> "}"
    prettyPrec _ (DefEffect k _ effName tvs ops) =
        "effect (" <> ppQName effName <+> "::" <+> pretty k <> ")" <+> encloseSep "" "" " " (map pretty tvs) <+> "{" <> line_
        <> indent_ (
            vsep (map (\(x, ty) -> ppQName x <> " :: " <> pretty ty) ops)
        ) <> line_ <> "}"


prettyDecl :: Decl SemAnalysis -> Doc ann
prettyDecl (Decl _ f xs e) = ppQName f <+> encloseSep "" "" " " (map ppQName xs) <+> "=" <> line_ <> indent_ (pretty e)


instance Pretty TVar where
    prettyPrec p (MkTVar var k) =
        let Config { printKinds } = getConfig () in
        if printKinds then
            prettyParen p SigPrec $ ppQName var <+> "::" <+> pretty k
        else
            ppQName var

instance Pretty (Expr SemAnalysis) where
    prettyPrec _ _ = "{TODO: Prettyprint expresions}"

instance Pretty Type where
    -- TODO
    prettyPrec _ = pretty . ppType

instance Pretty Kind where
    prettyPrec _ KStar = "*"
    prettyPrec _ KConstraint = "Constraint"
    prettyPrec _ KEffect = "Effect"
    prettyPrec p (KRow k) = prettyParen p AppPrec $ "KRow" <+> prettyPrec AtomPrec k 
    prettyPrec p (KFun k1 k2) = prettyParen p FunPrec $ prettyPrec AppPrec k1 <+> "->" <> prettyPrec FunPrec k2

instance Pretty (Module Typecheck) where
    prettyPrec _ m = pretty (coercePass @(Module Typecheck) @(Module SemAnalysis) m)


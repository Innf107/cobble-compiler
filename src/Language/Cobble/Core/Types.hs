module Language.Cobble.Core.Types where

import Language.Cobble.Prelude
import Language.Cobble.Types.QualifiedName

import Prettyprinter

import GHC.Show qualified as S

data Decl = Def QualifiedName Type Expr
          | DefVariant QualifiedName [(QualifiedName, Kind)] [(QualifiedName, [Type])] 
          deriving (Eq, Generic, Data)

data Expr = Var QualifiedName
          | App Expr Expr
          | TyApp Expr Type
          | Abs QualifiedName Type Expr
          | TyAbs QualifiedName Kind Expr

          | IntLit Int
          | UnitLit -- Unit should ideally just be a regular variant constructor, but hard-wiring libraries into the compiler is not really possible yet.
          | Let QualifiedName Type Expr Expr
          | If Expr Expr Expr
          -- Unlike in Cobble, Core's VariantConstrs have to be *fully saturated* with value and type arguments.
          | VariantConstr QualifiedName Int (Seq Type) (Seq Expr)

          | Join QualifiedName (Seq (QualifiedName, Kind)) (Seq (QualifiedName, Type)) Expr Expr -- Create a join point (See note [Join Points])
          --                    ^ type parameters           ^ value parameters
          | Jump QualifiedName (Seq Type) (Seq Expr) Type -- Tailcall into a join point (See note [Join Points])
          --                    ^         ^value args^           
          --                    | type args          | result type
          deriving (Eq, Generic, Data)

data Type = TVar QualifiedName Kind
          | TCon QualifiedName Kind
          | TFun Type Type
          | TApp Type Type
          | TForall QualifiedName Kind Type
          deriving (Eq, Generic, Data)

data Kind = KType
          | KFun Kind Kind
          deriving (Eq, Generic, Data)

{- Note [Join Points]
When lowering case expressions, or after certain optimizations, large expressions might
be duplicated. We can use a local let-binding that is called in tail position to mitigate the 
effect of this, but that would require allocating a closure for functions that are immediately 
invoked. 
Join points are exactly theses kinds of local bindings, except they may only be called in tail position and
don't need to allocate a closure. Join points are blazingly fast, since they can be compiled to unconditional jumps.

Unfortunately, join points introduce quite a bit of complexity, since typing them is not trivial, 
and we have to make sure that `jump` expressions only ever occur in tail positions.

Source: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/join-points-pldi17.pdf
-}

ppQName :: QualifiedName -> Doc ann
ppQName = pretty . renderDebug

prettyTyped :: (Pretty p) => (QualifiedName, p) -> Doc ann
prettyTyped (x, p) = ppQName x <+> ":" <+> pretty p

instance {-# OVERLAPPING #-} Pretty [Decl] where
    pretty ds = vsep (map pretty ds)

instance Pretty Decl where
    pretty (Def x ty e) = ppQName x <+> ":" <+> pretty ty <> line <> ppQName x <+> "=" <+> align (pretty e)
    pretty (DefVariant x args clauses) = "variant" <+> ppQName x <+> sep (map (\(x, k) -> "(" <> ppQName x <+> ":" <+> pretty k <> ")") args) <+> align ("="
                                    <+> (vsep (zipWith (\bar (c, args) -> (if bar then ("|" <+>) else id) (ppQName c) <+> sep (map pretty args)) (False:repeat True) clauses)))

instance Pretty Expr where
    pretty (Var x) = ppQName x
    pretty (App e1 e2) = "(" <> pretty e1 <+> pretty e2 <> ")"
    pretty (TyAbs tv k e) = "(Λ(" <> ppQName tv <+> ":" <+> pretty k <> ")." <+> align (pretty e) <> ")"
    pretty (TyApp e ty) = "(" <> pretty e <+> "@" <> pretty ty <> ")"
    pretty (Abs x ty e) = "(λ(" <> ppQName x <+> ":" <+> pretty ty <> ")." <+> align (pretty e) <> ")"
    pretty (IntLit n) = pretty n
    pretty UnitLit = "()"
    pretty (Let x ty e1 e2) = "(let" <+> ppQName x <+> ":" <+> pretty ty <+> "=" <+> align (pretty e1) <+> "in" <+> pretty e2 <+> ")"
    pretty (If c th el) = "(if" <+> pretty c <+> "then" <+> pretty th <+> "else" <+> pretty el <> ")"
    pretty (VariantConstr x _ tys es) = ppQName x <> tyApps <> valApps
        where
            tyApps = case tys of
                Empty -> ""
                (tys :|> ty) -> "[" <> foldl' (\r ty -> pretty ty <> "," <+> r) (pretty ty) tys <> "]"
            valApps = case es of
                Empty -> ""
                (es :|> e) -> "{" <> foldl' (\r e -> pretty e <> "," <+> r) (pretty e) es <> "}"
    pretty (Join j tyParams valParams body e) = "(join" <+> ppQName j 
                                                        <+> encloseSep "[" "]" ", " (map prettyTyped $ toList tyParams)
                                                        <+> encloseSep "{" "}" ", " (map prettyTyped $ toList tyParams)
                                                        <+> "="
                                                        <+> align (pretty body)
                                                        <+> "in"
                                                        <+> pretty e
                                                        <> ")"
    pretty (Jump j tyArgs valArgs retTy) = "(jump" <+> ppQName j <+> encloseSep "[" "]" "," (map pretty $ toList tyArgs)
                                                                 <+> encloseSep "{" "}" "," (map pretty $ toList valArgs)
                                                                 <+> pretty retTy
                                                                 <> ")"

instance Pretty Type where
    pretty (TVar x k) = ppQName x
    pretty (TCon x k) = "(" <> ppQName x <+> ":" <+> pretty k <> ")"
    pretty (TFun t1 t2) = "(" <> pretty t1 <+> "->" <+> pretty t2 <> ")"
    pretty (TApp t1 t2) = "(" <> pretty t1 <+> pretty t2 <> ")"
    pretty (TForall x k ty) = "(forall" <+> "(" <> ppQName x <+> ":" <+> pretty k <> ")." <+> pretty ty <> ")"
instance Pretty Kind where
    pretty (KType) = "*"
    pretty (KFun KType t2) = "*" <+> "->" <+> pretty t2
    pretty (KFun t1 t2) = "(" <> pretty t1 <> ")" <+> "->" <+> pretty t2

instance S.Show Decl where
    show = show . pretty
instance S.Show Expr where
    show = show . pretty
instance S.Show Type where
    show = show . pretty
instance S.Show Kind where
    show = show . pretty

-- Does not handle kinds yet
replaceTVar :: QualifiedName -> Type -> Type -> Type
replaceTVar tv substTy ty@(TVar tv' _)
    | tv == tv' = substTy
    | otherwise = ty
replaceTVar tv substTy ty@(TForall tv' k ty')
    | tv == tv' = ty
    | otherwise = TForall tv' k (replaceTVar tv substTy ty')
replaceTVar _ _ ty@TCon{} = ty
replaceTVar tv substTy (TApp t1 t2) = TApp (replaceTVar tv substTy t1) (replaceTVar tv substTy t2)
replaceTVar tv substTy (TFun t1 t2) = TFun (replaceTVar tv substTy t1) (replaceTVar tv substTy t2)


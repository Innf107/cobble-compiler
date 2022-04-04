module Language.Cobble.Core.Types where

import Language.Cobble.Prelude
import Language.Cobble.Types.QualifiedName

import Language.Cobble.Codegen.PrimOp

import Language.Cobble.Util.Prettyprinter
import Prettyprinter.Render.String

import GHC.Show qualified as S

data Decl = Def QualifiedName Type Expr
          | DefVariant QualifiedName (Seq (QualifiedName, Kind)) (Seq (QualifiedName, Seq Type))
          | DefDict QualifiedName (Seq (QualifiedName, Kind)) (Seq (QualifiedName, Type))
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

            --  Core Scrutinees have to be in WHNF, which makes it much easier to, say, fully eliminate a case expression during optimizations
          | Case QualifiedName (Seq (Pattern, Expr))

          | Join QualifiedName (Seq (QualifiedName, Kind)) (Seq (QualifiedName, Type)) Expr Expr -- Create a join point (See note [Join Points])
          --                    ^ type parameters           ^ value parameters
          | Jump QualifiedName (Seq Type) (Seq Expr) Type -- Tailcall into a join point (See note [Join Points])
          --                    ^         ^value args^           
          --                    | type args          | result type
          | PrimOp PrimOp Type (Seq Type) (Seq Expr)

          | DictConstruct QualifiedName (Seq Type) (Seq Expr)
          --              ^class name   ^class args ^fields
          | DictAccess Expr QualifiedName (Seq Type) QualifiedName
          --                ^class name   ^class args ^field
          deriving (Eq, Generic, Data)

data Pattern = PInt Int
             | PWildcard
             | PConstr QualifiedName (Seq (QualifiedName, Type)) Int
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

intTy :: Type
intTy = TCon (internalQName "Int") KType

boolTy :: Type
boolTy = TCon (internalQName "Bool") KType

unitTy :: Type
unitTy = TCon (internalQName "Unit") KType

ppQName :: QualifiedName -> Doc ann
ppQName = pretty . renderDebug

prettyTyped :: (Pretty p) => (QualifiedName, p) -> Doc ann
prettyTyped (x, p) = ppQName x <+> ":" <+> pretty p

prettyDecls ds = vsep (map ((<> line) . pretty) ds)

instance Pretty Decl where
    pretty (Def x ty e) = ppQName x <+> ":" <+> pretty ty <> line <> ppQName x <+> "=" <+> align (pretty e)
    pretty (DefVariant x args clauses) = "variant" <+> ppQName x <+> sep (map (\(x, k) -> "(" <> ppQName x <+> ":" <+> pretty k <> ")") args) <+> align ("="
                                    <+> (vsep (mapAfter1 False True (\bar (c, args) -> (if bar then ("|" <+>) else id) (ppQName c) <+> sep (map pretty args)) clauses)))
        where
            mapAfter1 :: a -> a -> (a -> b -> c) -> Seq b -> Seq c
            mapAfter1 a1 a2 f Empty = Empty
            mapAfter1 a1 a2 f (x :<| xs) = f a1 x :<| map (f a2) xs
    pretty (DefDict x args fields) = "dictionary" <+> ppQName x <+> sep (map (parens . prettyTyped) args) <+> "=" <+>
        align ("{" <> vsep (map prettyTyped fields) <> "}")

instance Pretty Expr where
    pretty (Var x) = ppQName x
    pretty (App e1 e2) = "(" <> pretty e1 <+> pretty e2 <> ")"
    pretty (TyAbs tv k e) = "(Λ(" <> ppQName tv <+> ":" <+> pretty k <> ")." <> softline <> align (pretty e) <> ")"
    pretty (TyApp e ty) = "(" <> pretty e <+> "@" <> pretty ty <> ")"
    pretty (Abs x ty e) = "(λ(" <> ppQName x <+> ":" <+> pretty ty <> ")." <> softline <> align (pretty e) <> ")"
    pretty (IntLit n) = pretty n
    pretty UnitLit = "()"
    pretty (Let x ty e1 e2) = "(let" <+> ppQName x <+> ":" <+> pretty ty <+> "=" <+> align (softline' <> pretty e1) <+> "in" <> line <> pretty e2 <> ")"
    pretty (If c th el) = "(if" <+> pretty c <> softline <> "then" <+> pretty th <> softline <> "else" <+> pretty el <> ")"
    pretty (VariantConstr x _ tys es) = ppQName x <> tyApps <> valApps
        where
            tyApps = case tys of
                Empty -> ""
                (tys :|> ty) -> "[" <> foldl' (\r ty -> pretty ty <> "," <+> r) (pretty ty) tys <> "]"
            valApps = case es of
                Empty -> ""
                (es :|> e) -> "{" <> foldl' (\r e -> pretty e <> "," <+> r) (pretty e) es <> "}"

    pretty (Case scrut branches) = "(case" <+> ppQName scrut <+> "of" <> 
                                    align (line <> vsep (map (\(p, e) -> pretty p <+> "->" <+> pretty e) branches))
                                    <> ")"


    pretty (Join j tyParams valParams body e) = "(join" <+> ppQName j 
                                                        <+> encloseSep "[" "]" ", " (map prettyTyped tyParams)
                                                        <+> encloseSep "{" "}" ", " (map prettyTyped valParams)
                                                        <+> "="
                                                        <+> align (softline' <> pretty body)
                                                        <+> "in"
                                                        <> line
                                                        <> pretty e
                                                        <> ")"
    pretty (Jump j tyArgs valArgs retTy) = "(jump" <+> ppQName j <+> encloseSep "[" "]" ", " (map pretty tyArgs)
                                                                 <+> encloseSep "{" "}" ", " (map pretty valArgs)
                                                                 <+> pretty retTy
                                                                 <> ")"
    -- the concrete primop type is not displayed since it should be obvious from the corresponding PrimOpInfo entry.
    pretty (PrimOp op ty tyArgs valArgs) = show op <> encloseSep "[" "]" ", " (map pretty tyArgs)
                                                          <> encloseSep "{" "}" ", " (map pretty valArgs)

    pretty (DictConstruct cname args fields) = ppQName cname <> encloseSep "[" "]" ", " (map pretty args)
                                                             <> encloseSep "{" "}" (line <> ", ") (map pretty fields)

    pretty (DictAccess e cname args field) = pretty e <> encloseSep ".[" "]." " " (ppQName cname :<| map pretty args) 
                                                      <> show field

instance Pretty Pattern where
    pretty (PInt i) = pretty i
    pretty PWildcard = "_"
    pretty (PConstr cname args i) = encloseSep "(" ")" " " (ppQName cname :<| (map (parens . prettyTyped) args))

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
instance S.Show Pattern where
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


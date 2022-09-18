module Cobble.Core.Syntax where

import Cobble.Prelude
import Cobble.Syntax.QualifiedName
import Cobble.Config(Config(..), getConfig)

import Cobble.Codegen.PrimOp

import Cobble.Util.Prettyprinter
import Prettyprinter.Render.String

import GHC.Show qualified as S

data Decl = Def QualifiedName Type Expr
          | DefVariant QualifiedName (Seq (QualifiedName, Kind)) (Seq (QualifiedName, Seq Type))
          | DefDict QualifiedName (Seq (QualifiedName, Kind)) (Seq (QualifiedName, Type))
          | DefEffect QualifiedName (Seq (QualifiedName, Kind)) (Seq (QualifiedName, Type))
          deriving (Eq, Generic, Data)
instance Binary Decl

data Expr = Var QualifiedName
          | App Expr Expr
          | TyApp Expr Type
          | Abs QualifiedName Effect Type Expr
          | TyAbs QualifiedName Kind Expr

          | IntLit Int
          | Let QualifiedName Type Expr Expr
          | If Expr Expr Expr
          -- Unlike in Cobble, Core's VariantConstrs have to be *fully saturated* with value and type arguments.
          | VariantConstr QualifiedName Int (Seq Type) (Seq Expr)

            --  Core Scrutinees have to be in HNF, which makes it much easier to, say, fully eliminate a case expression during optimizations
          | Case QualifiedName (Seq (Pattern, Expr))

          | Join QualifiedName (Seq (QualifiedName, Kind)) (Seq (QualifiedName, Type)) Expr Expr -- Create a join point (See note [Join Points])
          --                    ^ type parameters           ^ value parameters
          | Jump QualifiedName (Seq Type) (Seq Expr) Type -- Tailcall into a join point (See note [Join Points])
          --                    ^         ^value args^           
          --                    | type args          | result type
          | DictConstruct QualifiedName (Seq Type) (Seq Expr)
          --              ^class name   ^class args ^fields
          | DictAccess Expr QualifiedName (Seq Type) QualifiedName
          --                ^class name   ^class args ^field
          
          -- 'perform' operations have to be fully applied, just like primOps. 
          | Perform QualifiedName QualifiedName (Seq Type) (Seq Expr)
          --        ^effect       ^operation    ^type args ^value args 
          | Handle Expr Effect (Seq (QualifiedName, Seq QualifiedName, Expr)) (QualifiedName, Expr)
          --                   ^     ^operation     ^args              ^body  ^return clause
          --                   | handlers
          | Resume Expr -- TODO: Probably needs some type annotations somewhere?
          deriving (Eq, Generic, Data)
instance Binary Expr

data Pattern = PInt Int
             | PWildcard
             | PConstr QualifiedName (Seq (QualifiedName, Type)) Int
             deriving (Eq, Generic, Data)
instance Binary Pattern

data Type = TVar QualifiedName Kind             -- α            type variable
          | TCon QualifiedName Kind             -- C            type constructor
          | TFun Type Effect Type               -- σ -{ϵ}> σ    function type
          | TApp Type Type                      -- σ σ          type application
          | TForall QualifiedName Kind Type     -- ∀(α : κ). σ  forall types
          | TRowNil                             -- ⟨⟩           empty row
          | TRowExtend (Seq Type) Type          -- ⟨σ* | σ⟩     row extension
          | TEffUR                              -- ∞            unrestricted effect type (kind: row effect)
          deriving (Eq, Generic, Data)
instance Binary Type

type Effect = Type

data Kind = KType
          | KEffect
          | KRow Kind
          | KFun Kind Kind
          deriving (Eq, Generic, Data)
instance Binary Kind

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
boolTy = TCon (UnsafeQualifiedName "Bool" (GlobalQName "Data.Bool")) KType

ppQName :: QualifiedName -> Doc ann
ppQName = pretty . renderDebug

prettyTyped :: (Pretty p) => (QualifiedName, p) -> Doc ann
prettyTyped (x, p) = ppQName x <+> ":" <+> pretty p

prettyDecls :: Seq Decl -> Doc ann
prettyDecls ds = vsep (map ((<> line) . pretty) ds)

instance Pretty Decl where
    prettyPrec _ (Def x ty e) = ppQName x <+> ":" <+> pretty ty <> line <> ppQName x <+> "=" <+> align (pretty e)
    prettyPrec _ (DefVariant x args clauses) = "variant" <+> ppQName x <+> sep (map (\(x, k) -> prettyWithKind (ppQName x) k) args) <+> align ("="
                                    <+> (vsep (mapAfter1 False True (\bar (c, args) -> (if bar then ("|" <+>) else id) (ppQName c) <+> sep (map pretty args)) clauses)))
        where
            mapAfter1 :: a -> a -> (a -> b -> c) -> Seq b -> Seq c
            mapAfter1 a1 a2 f Empty = Empty
            mapAfter1 a1 a2 f (x :<| xs) = f a1 x :<| map (f a2) xs
    prettyPrec _ (DefDict x args fields) = "dictionary" <+> ppQName x <+> sep (map (parens . prettyTyped) args) <+> "=" <+>
        align ("{" <> vsep (map prettyTyped fields) <> "}")
    prettyPrec _ (DefEffect x args ops) = "effect" <+> ppQName x <+> sep (map (parens . prettyTyped) args) 
        <+> align ("{" <> vsep (map prettyTyped ops) <> "}")

instance Pretty Expr where
    prettyPrec _ (Var x) = ppQName x
    prettyPrec p (App e1 e2) = 
        prettyParen p AppPrec $ prettyPrec AppPrec e1 <+> prettyPrec AtomPrec e2
    prettyPrec p (TyAbs tv k e) = 
        prettyParen p LetPrec $
            "Λ" <> prettyWithKind (ppQName tv) k <> "." <> softline <> align (prettyPrec LetPrec e)
    prettyPrec p (TyApp e ty) = 
        prettyParen p AppPrec $
            prettyPrec AppPrec e <+> "@" <> prettyPrec AtomPrec ty
    prettyPrec p (Abs x eff ty e) = 
        prettyParen p LetPrec $
            "λ[" <> prettyPrec LetPrec eff <> "](" <> ppQName x <+> ":" <+> prettyPrec FunPrec ty <> ")." <> softline 
            <> align (prettyPrec LetPrec e)
    prettyPrec _ (IntLit n) = pretty n
    prettyPrec p (Let x ty e1 e2) =
        prettyParen p LetPrec $
            "let" <+> ppQName x <+> ":" <+> prettyPrec FunPrec ty <+> "=" 
                <+> align (softline' <> prettyPrec FunPrec e1) <+> "in" 
                <> line <> prettyPrec LetPrec e2
    prettyPrec p (If c th el) = 
        prettyParen p LetPrec $
            "if" <+> prettyPrec AppPrec c <> softline <> "then" 
                <+> prettyPrec LetPrec th <> softline <> "else" 
                <+> prettyPrec LetPrec el
    prettyPrec p (VariantConstr x _ tys es) = 
        prettyParen p AppPrec $
            ppQName x <> tyApps <> valApps
        where
            tyApps = case tys of
                Empty -> ""
                (tys :|> ty) -> "[" <> foldl' (\r ty -> prettyPrec FunPrec ty <> "," <+> r) (prettyPrec FunPrec ty) tys <> "]"
            valApps = case es of
                Empty -> ""
                (es :|> e) -> "{" <> foldl' (\r e -> prettyPrec FunPrec e <> "," <+> r) (prettyPrec FunPrec e) es <> "}"

    prettyPrec p (Case scrut branches) = 
        prettyParen p LetPrec $
            "case" <+> ppQName scrut <+> "of" 
                    <> align (line <> vsep (map (\(p, e) -> prettyPrec AppPrec p <+> "->" <+> prettyPrec LetPrec e) branches))


    prettyPrec p (Join j tyParams valParams body e) = 
        prettyParen p LetPrec $
            "join" <+> ppQName j 
                   <+> encloseSep "[" "]" ", " (map prettyTyped tyParams)
                   <+> encloseSep "{" "}" ", " (map prettyTyped valParams)
                   <+> "="
                   <+> align (softline' <> prettyPrec FunPrec body)
                   <+> "in"
                   <> line
                   <> prettyPrec LetPrec e
                   
    prettyPrec p (Jump j tyArgs valArgs retTy) = 
        prettyParen p AppPrec $ 
            "jump" <+> ppQName j 
                   <+> encloseSep "[" "]" ", " (map (prettyPrec FunPrec) tyArgs)
                   <+> encloseSep "{" "}" ", " (map (prettyPrec FunPrec) valArgs)
                   <+> prettyPrec AppPrec retTy
                                                                 
    prettyPrec p (DictConstruct cname args fields) = 
        prettyParen p AppPrec $
            ppQName cname <> encloseSep "[" "]" ", " (map (prettyPrec FunPrec) args)
                          <> encloseSep "{" "}" (line <> ", ") (map (prettyPrec FunPrec) fields)

    prettyPrec p (DictAccess e cname args field) = 
        prettyParen p AppPrec $
        pretty e <> encloseSep ".[" "]." " " (ppQName cname :<| map (prettyPrec FunPrec) args) <> ppQName field

    prettyPrec p (Perform effect op tyArgs valArgs) = 
        prettyParen p AppPrec $
            "perform[" <> ppQName effect <> "]" <+> ppQName op <+> encloseSep "[" "]" ", " (map (prettyPrec FunPrec) tyArgs)
                    <> encloseSep "{" "}" ", " (map (prettyPrec FunPrec) valArgs)
    prettyPrec p (Handle e eff handlers (retVar, retExpr)) = 
        prettyParen p LetPrec $
            "handle[" <> pretty eff <> "]" <+> pretty e <+> "with" 
                                    <> line <> align (indent 4 (
                                            vsep (map prettyHandler handlers)
                                        <>  line <> "return" <+> ppQName retVar <+> "->" <> prettyPrec LetPrec retExpr
                                    ))
                                    where
                                        prettyHandler (op, args, expr) = 
                                            ppQName op <+> encloseSep "" "" " " (map ppQName args)
                                            <+> "->" <+> prettyPrec LetPrec expr
    prettyPrec p (Resume expr) = 
        prettyParen p AppPrec $
            "resume" <+> pretty expr


instance Pretty Pattern where
    prettyPrec _ (PInt i) = pretty i
    prettyPrec _ PWildcard = "_"
    prettyPrec _ (PConstr cname [] _) = ppQName cname 
    prettyPrec p (PConstr cname args _) = 
        prettyParen p AppPrec $ sep (ppQName cname :<| (map (parens . prettyTyped) args))

instance Pretty Type where
    prettyPrec p (TVar x k) = 
        let Config { printConstructorKinds } = getConfig () in
        if printConstructorKinds then
            prettyParen p SigPrec $ ppQName x <+> ":" <+> prettyPrec FunPrec k
        else
            ppQName x
    prettyPrec p (TCon x k) =
        let Config { printConstructorKinds } = getConfig () in
        if printConstructorKinds then
            prettyParen p SigPrec $ ppQName x <+> ":" <+> prettyPrec FunPrec k
        else
            ppQName x
    prettyPrec p (TFun t1 TRowNil t2) =
        prettyParen p FunPrec $ prettyPrec AppPrec t1 <+> "->" <+> prettyPrec FunPrec t2
    prettyPrec p (TFun t1 eff t2) = 
        prettyParen p FunPrec $ prettyPrec AppPrec t1 <+> "-{" <> prettyPrec LetPrec eff <> "}>" <+> prettyPrec FunPrec t2
    prettyPrec p (TApp t1 t2) = 
        prettyParen p AppPrec $ prettyPrec AppPrec t1 <+> prettyPrec AtomPrec t2
    prettyPrec p ty@TForall{} = do
        let go = \case
                (TForall x k ty) -> first ((x, k) <|) $ go ty
                ty -> ([], ty)
        let (consecutiveForalls, resultTy) = go ty
        prettyParen p FunPrec $ "∀" <> sep (map (\(x, k) -> prettyWithKind (ppQName x) k) consecutiveForalls) 
            <> "." <+> prettyPrec FunPrec resultTy
    prettyPrec _ TRowNil = "{}"
    prettyPrec _ (TRowExtend tys row) = "{" <> fold (intersperse ", " (map pretty tys)) <> " | " <> pretty row <> "}"
    prettyPrec _ TEffUR = "∞"
instance Pretty Kind where
    prettyPrec _ KType = "*"
    prettyPrec _ KEffect = "Effect"
    prettyPrec p (KRow k) = prettyParen p AppPrec $ "Row" <+> prettyPrec AtomPrec k
    prettyPrec p  (KFun t1 t2) = prettyParen p FunPrec $ prettyPrec AppPrec t1 <+> "->" <+> prettyPrec LetPrec t2

prettyWithKind :: Doc ann -> Kind -> Doc ann
prettyWithKind name KType = name
prettyWithKind name k = 
    let Config { printKinds } = getConfig () in
    if printKinds then
        "(" <> name <+> ":" <+> prettyPrec AppPrec k <> ")"
    else
        name

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
replaceTVar tv substTy (TFun t1 effs t2) = TFun (replaceTVar tv substTy t1) (replaceTVar tv substTy effs) (replaceTVar tv substTy t2)
replaceTVar tv substTy TRowNil = TRowNil
replaceTVar tv substTy (TRowExtend tys row) = TRowExtend (map (replaceTVar tv substTy) tys) (replaceTVar tv substTy row)
replaceTVar tv substTy TEffUR = TEffUR

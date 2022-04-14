{-# LANGUAGE UndecidableInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints#-}
module Cobble.Types.AST where

import Cobble.Prelude
import Cobble.Util.TypeUtils

import Cobble.Types.TH

import Cobble.Types.LexInfo
import Cobble.Types.QualifiedName

import Data.Generics.Uniplate.Data

import GHC.Show qualified as S

import Data.Text qualified as T

import qualified Unsafe.Coerce 

import qualified Data.Map as M
import qualified Data.Set as Set

type family Name (p :: Pass)

type family InstanceRequirements t :: [HSType]

-- Top level module.
data Module (p :: Pass) = Module
    { xModule :: (XModule p)
    , moduleName :: Text
    , moduleStatements :: (Seq (Statement p))
    }


type instance InstanceRequirements (Module p) = [XModule p, Name p, Statement p]

type family XModule (p :: Pass)

-- | The signature of a module contains
-- everything that it exports
-- (Variables, Functions, Types, etc.)
data ModSig = ModSig {
    exportedVars            :: Map QualifiedName Type
,   exportedVariantConstrs  :: Map QualifiedName (Type, Int, Int, TypeVariant)
,   exportedTypes           :: Map QualifiedName (Kind, TypeVariant)
,   exportedFixities        :: Map QualifiedName Fixity
,   exportedInstances       :: Map QualifiedName (Seq (Type, QualifiedName))
} deriving (Show, Eq, Generic, Data, Typeable)

instance Binary ModSig

data TypeVariant = VariantType (Seq TVar) (Seq (QualifiedName, (Seq Type)))
                 | BuiltInType
                 | TyClass (Seq TVar) (Seq (QualifiedName, Type))
                 | TyEffect (Seq TVar) (Seq (QualifiedName, Type))
                 deriving (Show, Eq, Generic, Data, Typeable)

instance Binary TypeVariant

type Dependencies = Map QualifiedName ModSig

instance Semigroup ModSig where 
    ModSig vs cs ts fs is <> ModSig vs' cs' ts' fs' is' = 
        ModSig 
            (vs <> vs') 
            (cs <> cs') 
            (ts <> ts') 
            (fs <> fs') 
            (is `munion` is')

instance Monoid ModSig where mempty = ModSig mempty mempty mempty mempty mempty

-- | A data kind representing the state of the AST at a certain Compiler pass.
data Pass = SolveModules
          | QualifyNames
          | SemAnalysis
          | Typecheck
          | Codegen


data Statement (p :: Pass) =
      Def           (XDef p)            LexInfo (Decl p) (XType p)
    | Import        (XImport p)         LexInfo (Name p) -- TODO: qualified? exposing?
    | DefClass      (XDefClass p)       LexInfo (Name p) (Seq (XTVar p)) (Seq (Name p, XType p))
    | DefInstance   (XDefInstance p)    LexInfo (Name p) (XType p) (Seq (Decl p))
    | DefVariant    (XDefVariant p)     LexInfo (Name p) (Seq (XTVar p)) (Seq (Name p, (Seq (XType p)), XDefVariantClause p))
    | DefEffect     (XDefEffect p)      LexInfo (Name p) (Seq (XTVar p)) (Seq (Name p, XType p))
    | StatementX    (XStatement p)      LexInfo

type instance InstanceRequirements (Statement p) = 
    [XDef p, XImport p, XDefClass p, XDefInstance p, XDefVariant p, XDefVariantClause p, XDefEffect p, XStatement p, Name p, XType p, XTVar p, Decl p]

type family XDef                (p :: Pass)
type family XParam              (p :: Pass)
type family XImport             (p :: Pass)
type family XDefClass           (p :: Pass)
type family XDefInstance        (p :: Pass)
type family XDefVariant         (p :: Pass)
type family XDefVariantClause   (p :: Pass)
type family XDefEffect          (p :: Pass)
type family XStatement          (p :: Pass)


data Decl (p :: Pass) = Decl (XDecl p) (Name p) (XParam p) (Expr p)

type instance InstanceRequirements (Decl p) = [XDecl p, Name p, XParam p, Expr p]

type family XDecl (p :: Pass)

data Expr (p :: Pass) =
      App             (XFCall p) LexInfo (Expr p) (Expr p)
    | IntLit          (XIntLit p) LexInfo Int
    | If              (XIf p) LexInfo (Expr p) (Expr p) (Expr p)
    | Let             (XLet p) LexInfo (Decl p) (Expr p)
    | Var             (XVar p) LexInfo (Name p)
    | Ascription      (XAscription p) LexInfo (Expr p) (XType p)
    | VariantConstr   (XVariantConstr p) LexInfo (Name p)
    | Case            (XCase p) LexInfo (Expr p) (Seq (CaseBranch p))
    | Lambda          (XLambda p) LexInfo (Name p) (Expr p)
    | ExprX           (XExpr p) LexInfo

type instance InstanceRequirements (Expr p) = 
        [
            XFCall p, XIntLit p, XIf p, XLet p, Decl p, XVar p, XAscription p, XVariantConstr p, XCase p, CaseBranch p, 
            Name p, XType p, XLambda p, XExpr p
        ]

type family XFCall           (p :: Pass)
type family XIntLit          (p :: Pass)
type family XIf              (p :: Pass)
type family XLet             (p :: Pass)
type family XVar             (p :: Pass)
type family XAscription      (p :: Pass)
type family XVariantConstr   (p :: Pass)
type family XCase            (p :: Pass)
type family XLambda          (p :: Pass)
type family XExpr            (p :: Pass)

data CaseBranch (p :: Pass) = CaseBranch (XCaseBranch p) LexInfo (Pattern p) (Expr p)

type instance InstanceRequirements (CaseBranch p) = [
        XCaseBranch p, Pattern p, Expr p
    ]

type family XCaseBranch (p :: Pass)

data Pattern (p :: Pass) = IntP (XIntP p) Int
                         | VarP (XVarP p) (Name p)
                         | ConstrP (XConstrP p) (Name p) (Seq (Pattern p))
                         | WildcardP (XWildcardP p)
                         | OrP (XOrP p) (Seq (Pattern p))
                         | PatternX (XPattern p)

type instance InstanceRequirements (Pattern p) = [
        Name p, XIntP p, XVarP p, XConstrP p, XWildcardP p, XOrP p, XPattern p
    ]

data UnitLit = UnitLit

data CodegenExt = TyApp_ Type (Expr Codegen)
                | TyAbs_ TVar (Expr Codegen)
                | DictAbs_ QualifiedName Constraint (Expr Codegen)
                | DictVarApp_ (Expr Codegen) QualifiedName
                | DictApp_ (Expr Codegen) QualifiedName


pattern TyApp :: (XExpr p ~ CodegenExt) => LexInfo -> Type -> Expr Codegen -> Expr p
pattern TyApp li ty e = ExprX (TyApp_ ty e) li

pattern TyAbs :: (XExpr p ~ CodegenExt) => LexInfo -> TVar -> Expr Codegen -> Expr p
pattern TyAbs li tv e = ExprX (TyAbs_ tv e) li
-- makeCompleteX ''Expr ['TyApp, 'TyAbs] is defined at the bottom of this file.

pattern DictAbs :: (XExpr p ~ CodegenExt) => LexInfo -> QualifiedName -> Constraint -> Expr Codegen -> Expr p
pattern DictAbs li x c e = ExprX (DictAbs_ x c e) li

pattern DictVarApp :: (XExpr p ~ CodegenExt) => LexInfo -> Expr Codegen -> QualifiedName -> Expr p
pattern DictVarApp li e dv = ExprX (DictVarApp_ e dv) li

pattern DictApp :: (XExpr p ~ CodegenExt) => LexInfo -> Expr Codegen -> QualifiedName -> Expr p
pattern DictApp li e dv = ExprX (DictApp_ e dv) li


type instance InstanceRequirements (CodegenExt) = '[
        Expr Codegen
    ]

type family XIntP       (p :: Pass)
type family XVarP       (p :: Pass)
type family XConstrP    (p :: Pass)
type family XWildcardP  (p :: Pass)
type family XOrP        (p :: Pass)
type family XPattern    (p :: Pass)

data Type = TCon QualifiedName Kind                 -- c
          | TApp Type Type                          -- σ σ
          | TVar TVar                               -- α
          | TSkol QualifiedName TVar                -- #α
          --      ^             ^original
          --      |skolem
          | TForall (Seq TVar) Type                 -- ∀α*. σ
          | TFun Type Effect Type                   -- σ -{ϵ}> σ
          | TConstraint Constraint Type             -- Q => σ
          | TRowClosed (Seq Type)                   -- ⟨σ*⟩
          | TRowOpen (Seq Type) TVar                -- ⟨σ* | α⟩
          | TRowSkol (Seq Type) QualifiedName TVar  -- ⟨σ* | #α⟩
          deriving (Eq, Ord, Generic, Data)
instance Binary Type

isTRowAny :: Type -> Bool
isTRowAny TRowClosed{} = True
isTRowAny TRowOpen{} = True
isTRowAny TRowSkol{} = True
isTRowAny _ = False

pattern TRowAny :: Type
pattern TRowAny <- (isTRowAny -> True)


type Effect = Type

instance S.Show Type where 
    show = toString . (<>")") . ("(Type " <>) . ppType

ppType :: Type -> Text
ppType (TFun a (TRowClosed Empty) b) = "(" <> ppType a <> " -> " <> ppType b <> ")"
ppType (TFun a effs b)               = "(" <> ppType a <> " -{" <> ppType effs <> "}> " <> ppType b <> ")"
ppType (TVar (MkTVar v _))           = show v
ppType (TSkol v _)                   = "#" <> show v
ppType (TCon v _)                    = show v
ppType (TApp a b)                    = "(" <> ppType a <> " " <> ppType b <> ")"
ppType (TForall ps t)                = "(∀" <> T.intercalate " " (toList $ map (\(MkTVar v _) -> show v) ps) <> ". " <> ppType t <> ")"
ppType (TConstraint c t)             = ppConstraint c <> " => " <> ppType t
ppType (TRowClosed tys)              = "⟨" <> intercalate ", " (map ppType tys) <> "⟩"
ppType (TRowOpen tys var)            = "⟨" <> intercalate ", " (map ppType tys) <> " | " <> show var <> "⟩"
ppType (TRowSkol tys skolName var)   = "⟨" <> intercalate ", " (map ppType tys) <> " | " <> ppType (TSkol skolName var) <> "⟩"

ppConstraint :: Constraint -> Text
ppConstraint (MkConstraint n k t) = "(" <> show n <> " : " <> show k <> ")" <> ppType t

-- See @Type@ for descriptions
data UType = UTCon UnqualifiedName
           | UTApp UType UType
           | UTVar UnqualifiedName
           | UTForall (Seq (UnqualifiedName, Maybe Kind)) UType
           | UTFun UType UType UType
           | UTConstraint UConstraint UType
           | UTRowClosed (Seq UType)
           | UTRowOpen (Seq UType) UnqualifiedName
           deriving (Show, Eq, Generic, Data)

type family XType (p :: Pass) :: HSType

freeTVs :: Type -> Set TVar
freeTVs = go mempty
    where
        go bound (TCon _ _)         = mempty
        go bound (TApp a b)         = go bound a <> go bound b
        go bound (TVar tv) 
            | tv `Set.member` bound = mempty
            | otherwise             = one tv
        go bound (TSkol _ _)        = mempty
        go bound (TForall tvs ty)   = go (bound <> Set.fromList (toList tvs)) ty
        go bound (TFun a eff b)     = go bound a <> go bound eff <> go bound b 
        go bound (TConstraint (MkConstraint _ _ t1) t2) = go bound t1 <> go bound t2
        go bound (TRowClosed tys)   = foldMap (go bound) tys
        go bound (TRowOpen tys var) = foldMap (go bound) tys <> go bound (TVar var)
        go bound (TRowSkol tys _ _) = foldMap (go bound) tys

-- Like freeTVs, but preserves the order of free ty vars
freeTVsOrdered :: Type -> Seq TVar
freeTVsOrdered = ordNub . go mempty
    where
        go bound (TCon _ _)         = mempty
        go bound (TApp a b)         = go bound a <> go bound b
        go bound (TVar tv) 
            | tv `Set.member` bound = mempty
            | otherwise             = one tv
        go bound (TSkol _ _)        = mempty
        go bound (TForall tvs ty)   = go (bound <> Set.fromList (toList tvs)) ty
        go bound (TFun a effs b)    = go bound a <> go bound effs <> go bound b 
        go bound (TConstraint (MkConstraint _ _ t1) t2) = go bound t1 <> go bound t2
        go bound (TRowClosed tys)   = foldMap (go bound) tys
        go bound (TRowOpen tys var) = foldMap (go bound) tys <> go bound (TVar var)
        go bound (TRowSkol tys _ _) = foldMap (go bound) tys

data TVar = MkTVar QualifiedName Kind
          deriving (Show, Eq, Ord, Generic, Data)

instance Binary TVar

type family XTVar (p :: Pass) :: HSType

-- TODO: Ideally, @Constraint@ should be unnecessary and @TConstraint@
-- should use @Type@ instead. For now there would be no advantage to
-- this, so Constraint is implemented like this.
-- TODO: Also, MultiParam Typeclasses!
-- TODO: This should also really be a List of (Name, Type p), since one might have multiple constraints
data Constraint = MkConstraint QualifiedName Kind Type
                deriving (Show, Eq, Ord, Generic, Data)

instance Binary Constraint

data UConstraint = MkUConstraint UnqualifiedName UType 
                 deriving (Show, Eq, Generic, Data)

data Kind = KStar
          | KConstraint 
          | KEffect
          | KRow Kind
          | KFun Kind Kind 
          deriving (Eq, Ord, Generic, Data, Typeable)
infixr 5 `KFun`

instance Binary Kind

data TGiven  = TGiven  Constraint LexInfo deriving (Show, Eq, Generic, Data, Typeable)
data TWanted = TWanted Constraint LexInfo deriving (Show, Eq, Generic, Data, Typeable)

pattern (:->) :: Type -> Type -> Type
pattern (:->) t1 t2 = TFun t1 (TRowClosed Empty) t2
infixr 1 :->

type family XKind (p :: Pass)


-- | Combines two 'LexInfo's
-- This assumes that the first one comes before the second
-- and that they are both part of the same file.
mergeLexInfo :: LexInfo -> LexInfo -> LexInfo
mergeLexInfo (LexInfo {startPos, file}) (LexInfo {endPos}) = LexInfo {startPos, endPos, file}


-- | Represents a (initially left-associative) group of operators whose fixity has not been resolved yet.
data OperatorGroup (p :: Pass) (f :: FixityStatus) 
    = OpNode (OperatorGroup p f) (Name p, XFixity f) (OperatorGroup p f)
    | OpLeaf (Expr p)

-- Since OperatorGroup takes a second parameter, this has to be defined manually (for now)
deriving instance (AllC Show [Name p, XFixity f, Expr p]) => Show       (OperatorGroup p f)
deriving instance (AllC Eq   [Name p, XFixity f, Expr p]) => Eq         (OperatorGroup p f)
deriving instance                                            Generic    (OperatorGroup p f)
deriving instance (AllC Data [Name p, XFixity f, Expr p], Typeable (OperatorGroup p f)) => Data (OperatorGroup p f)

data FixityStatus = WithFixity | NoFixity
type family XFixity (f :: FixityStatus) where
    XFixity WithFixity = Fixity
    XFixity NoFixity   = () 


data Fixity = LeftFix Int | RightFix Int deriving (Show, Eq, Generic, Data)

instance Binary Fixity

getPrecedence :: Fixity -> Int
getPrecedence (LeftFix p)  = p
getPrecedence (RightFix p) = p

-- This is *not* just (<) from Ord, since this does not satisfy the Ord laws.
lowerPrecedence :: Fixity -> Fixity -> Bool
lowerPrecedence (LeftFix p) (LeftFix p')    = p <= p'
lowerPrecedence (RightFix p) (RightFix p')  = p <  p'
lowerPrecedence (LeftFix p) (RightFix p')   = p <= p'
lowerPrecedence (RightFix p) (LeftFix p')   = p <= p'

instance S.Show Kind where
    show KStar = "*"
    show KConstraint = "Constraint"
    show KEffect = "Effect"
    show (KRow k) = "(Kind " <> show k <> ")"
    show (KFun k1 k2) = "(" <> show k1 <> " -> " <> show k2 <> ")"

intT, boolT, unitT :: Type
intT  = TCon (internalQName "Int") KStar
boolT = TCon (UnsafeQualifiedName "Bool" (GlobalQName "Data.Bool")) KStar
unitT = TCon (UnsafeQualifiedName "Unit" (GlobalQName "Data.Unit")) KStar

class HasKind t where
    kind :: t -> Either (Kind, Kind) Kind

instance HasKind TVar where
    kind (MkTVar _ k) = Right k

instance HasKind Type where
    kind = \case
        TVar v -> kind v
        TSkol _ v -> kind v
        TCon _ k -> pure k
        TApp t1 t2 -> bisequence (kind t1, kind t2) >>= \case
            (KFun kp kr, ka)
                | kp == ka -> pure kr
            (k1, k2) -> Left (k1, k2)
        TFun t1 effs t2 -> pure KStar
        TForall _ t -> kind t
        TConstraint _ t -> kind t 
        TRowClosed _   -> Right KEffect -- TODO: Should we really hardcode row kinds to be effects here?
        TRowOpen _ tv -> kind tv
        TRowSkol _ _ tv -> kind tv

instance HasKind Constraint where
    kind _ = pure KConstraint 

-- | A class used to coerce types between compatible passes (i.e. passes where the types are equivalent).
-- Instances for this *should not* be written by hand, but generated by TemplateHaskell, since we have to ensure
-- that all constraints are included
class CoercePass a b
        
coercePass :: (CoercePass t1 t2) => t1 -> t2
coercePass = Unsafe.Coerce.unsafeCoerce

instance {-# INCOHERENT #-} (Coercible a b) => CoercePass a b

instance {-# INCOHERENT #-} (CoercePass k1 k2, CoercePass v1 v2, Ord k2) => CoercePass (Map k1 v1) (Map k2 v2)

instance {-#INCOHERENT#-} (CoercePass a b) => CoercePass [a] [b]
instance {-#INCOHERENT#-} (CoercePass a b) => CoercePass (Seq a) (Seq b)

instance {-# INCOHERENT #-} (CoercePass a a', CoercePass b b') => CoercePass (a, b) (a', b')
instance {-# INCOHERENT #-} (CoercePass a a', CoercePass b b', CoercePass c c') => CoercePass (a, b, c) (a', b', c')

instance {-# INCOHERENT #-} (CoercePass a a', CoercePass b b') => CoercePass (Either a b) (Either a' b')

deriveCoercePass ''Module
deriveCoercePass ''Statement
deriveCoercePass ''Expr
deriveCoercePass ''CaseBranch
deriveCoercePass ''Pattern
deriveCoercePass ''Decl

makeCompleteX ''Expr ['TyApp, 'TyAbs, 'DictAbs, 'DictVarApp, 'DictApp]

-- TODO: Cannot be generated with TH right now, since it depends on f
instance {-# INCOHERENT #-} (CoercePass (Name p1) (Name p2), CoercePass (Expr p1) (Expr p2)) => CoercePass (OperatorGroup p1 f) (OperatorGroup p2 f) where

class HasLexInfo t where
    getLexInfo :: t -> LexInfo

instance HasLexInfo (Expr p) where
    getLexInfo = \case
        App _ li _ _             -> li
        IntLit _ li _            -> li
        If _ li _ _ _            -> li
        Let _ li _ _             -> li
        Var _ li _               -> li
        Ascription _ li _ _      -> li
        VariantConstr _ li _     -> li
        Case _ li _ _            -> li
        Lambda _ li _ _          -> li
        ExprX _ li               -> li
        

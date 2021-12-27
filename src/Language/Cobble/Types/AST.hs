{-# LANGUAGE UndecidableInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints#-}
module Language.Cobble.Types.AST where

import Language.Cobble.Prelude
import Language.Cobble.Util.Convert
import Language.Cobble.Util.TypeUtils

import Language.Cobble.Types.TH

import Language.Cobble.Types.LexInfo
import Language.Cobble.Types.QualifiedName

import Data.Generics.Uniplate.Data

import GHC.Show qualified as S

import qualified Unsafe.Coerce 

import qualified Data.Map as M

type family Name (p :: Pass)

type family InstanceRequirements t :: [HSType]

-- Top level module.
data Module (p :: Pass) = Module
    { xModule :: (XModule p)
    , moduleName :: (Name p)
    , moduleStatements :: [Statement p]
    }


type instance InstanceRequirements (Module p) = [XModule p, Name p, Statement p]

type family XModule (p :: Pass)

-- | The signature of a module contains
-- everything that it exports
-- (Variables, Functions, Types, etc.)
data ModSig = ModSig {
    exportedVars            :: Map QualifiedName (Type 'Codegen)
,   exportedVariantConstrs  :: Map QualifiedName (Type 'Codegen, Int, Int)
,   exportedTypes           :: Map QualifiedName (Kind, TypeVariant)
,   exportedFixities        :: Map QualifiedName Fixity
,   exportedInstances       :: Map QualifiedName [Type Codegen]
} deriving (Generic, Typeable) -- Instances for @Eq@ and @Data@ are defined in Language.Cobble.Types.AST.Codegen

data TypeVariant = RecordType [TVar Codegen] [(UnqualifiedName, Type 'Codegen)]
                 | VariantType [TVar Codegen] [(QualifiedName, [Type Codegen])]
                 | BuiltInType
                 | TyClass [TVar Codegen] [(QualifiedName, Type Codegen)]
                 deriving (Generic, Typeable)

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
          | PostProcess
          | Codegen


data Statement (p :: Pass) =
      Def           (XDef p)            LexInfo (Decl p) (Type p)
    | Import        (XImport p)         LexInfo (Name p) -- TODO: qualified? exposing?
    | DefStruct     (XDefStruct p)      LexInfo (Name p) [(TVar p)] [(UnqualifiedName, Type p)]
    | DefClass      (XDefClass p)       LexInfo (Name p) [(TVar p)] [(Name p, Type p)]
    | DefInstance   (XDefInstance p)    LexInfo (Name p) (Type p) [Decl p]
    | DefVariant    (XDefVariant p)     LexInfo (Name p) [(TVar p)] [(Name p, [Type p], XDefVariantClause p)]
    | StatementX    (XStatement p)      LexInfo

type instance InstanceRequirements (Statement p) = 
    [XDef p, XImport p, XDefStruct p, XDefClass p, XDefInstance p, XDefVariant p, XDefVariantClause p, XStatement p, Name p, Type p, TVar p, Decl p]

type family XDef                (p :: Pass)
type family XParam              (p :: Pass)
type family XImport             (p :: Pass)
type family XDefStruct          (p :: Pass)
type family XDefClass           (p :: Pass)
type family XDefInstance        (p :: Pass)
type family XDefVariant         (p :: Pass)
type family XDefVariantClause   (p :: Pass)
type family XStatement          (p :: Pass)


data Decl (p :: Pass) = Decl (XDecl p) (Name p) (XParam p) (Expr p)

type instance InstanceRequirements (Decl p) = [XDecl p, Name p, XParam p, Expr p]

type family XDecl (p :: Pass)

data Expr (p :: Pass) =
      FCall           (XFCall p) LexInfo (Expr p) (NonEmpty (Expr p))
    | IntLit          (XIntLit p) LexInfo Int
    | UnitLit         LexInfo
    | If              (XIf p) LexInfo (Expr p) (Expr p) (Expr p)
    | Let             (XLet p) LexInfo (Decl p) (Expr p)
    | Var             (XVar p) LexInfo (Name p)
    | Ascription      (XAscription p) LexInfo (Expr p) (Type p)
    | VariantConstr   (XVariantConstr p) LexInfo (Name p)
    | Case            (XCase p) LexInfo (Expr p) [CaseBranch p]
    | StructConstruct (XStructConstruct p) LexInfo (Name p) [(UnqualifiedName, Expr p)]
    | StructAccess    (XStructAccess p) LexInfo (Expr p) UnqualifiedName
    | Lambda          (XLambda p) LexInfo (Name p) (Expr p)
    | ExprX           (XExpr p) LexInfo

type instance InstanceRequirements (Expr p) = 
        [
            XFCall p, XIntLit p, XIf p, XLet p, Decl p, XVar p, XAscription p, XVariantConstr p, XCase p, CaseBranch p, 
            Name p, Type p, XStructConstruct p, XStructAccess p, XLambda p, XExpr p
        ]

type family XFCall           (p :: Pass)
type family XIntLit          (p :: Pass)
type family XIf              (p :: Pass)
type family XLet             (p :: Pass)
type family XVar             (p :: Pass)
type family XAscription      (p :: Pass)
type family XVariantConstr   (p :: Pass)
type family XCase            (p :: Pass)
type family XStructConstruct (p :: Pass)
type family XStructAccess    (p :: Pass)
type family XLambda          (p :: Pass)
type family XExpr            (p :: Pass)

data CaseBranch (p :: Pass) = CaseBranch (XCaseBranch p) LexInfo (Pattern p) (Expr p)

type instance InstanceRequirements (CaseBranch p) = [
        XCaseBranch p, Pattern p, Expr p
    ]

type family XCaseBranch (p :: Pass)

data Pattern (p :: Pass) = IntP (XIntP p) Int
                         | VarP (XVarP p) (Name p)
                         | ConstrP (XConstrP p) (Name p) [Pattern p]
                         | PatternX (XPattern p)

type instance InstanceRequirements (Pattern p) = [
        Name p, XIntP p, XVarP p, XConstrP p, XPattern p
    ]

type family XIntP       (p :: Pass)
type family XVarP       (p :: Pass)
type family XConstrP    (p :: Pass)
type family XPattern    (p :: Pass)

data Kind = KStar           
          | KConstraint 
          | KFun Kind Kind 
          deriving (Eq, Ord, Generic, Data, Typeable)

infixr 5 `KFun`

data Type (p :: Pass) = TCon (Name p) (XKind p)
                      | TApp (Type p) (Type p)
                      | TVar (TVar p)
                      | TSkol (TVar p)
                      | TForall [TVar p] (Type p)
                      | TConstraint (Constraint p) (Type p)

data TVar (p :: Pass) = MkTVar (Name p) (XKind p)

-- TODO: Ideally, @Constraint@ should be unnecessary and @TConstraint@
-- should use @Type@ instead. For now there would be no advantage to
-- this, so Constraint is implemented like this.
-- TODO: Also, MultiParam Typeclasses!
-- TODO: This should also really be a List of (Name, Type p), since you might have multiple constraints
data Constraint p = MkConstraint (Name p) (Type p)

data TGiven  = TGiven  (Constraint PostProcess) LexInfo
data TWanted = TWanted (Constraint PostProcess) LexInfo

type instance InstanceRequirements (Type p) = [Name p, XKind p, TVar p]
type instance InstanceRequirements (TVar p) = [Name p, XKind p]
type instance InstanceRequirements (Constraint p) = [Name p, XKind p, Type p]

(-:>) :: (TyLit (Name p), IsKind (XKind p)) => Type p -> Type p -> Type p
t1 -:> t2 = TApp (TApp (TCon tyFunT (kFun kStar (kFun kStar kStar))) t1) t2
infixr 5 -:>

pattern (:->) :: (Name p ~ QualifiedName, Eq (Name p), XKind p ~ Kind) => Type p -> Type p -> Type p
pattern (:->) t1 t2 = TApp (TApp (TCon (UnsafeQualifiedName "->" 0 InternalLexInfo) (KFun KStar (KFun KStar KStar))) t1) t2
infixr 1 :->

pattern (:~>) :: (Name p ~ Text, Eq (Name p), XKind p ~ Kind) => Type p -> Type p -> Type p
pattern (:~>) t1 t2 = TApp (TApp (TCon "->" (KFun KStar (KFun KStar KStar))) t1) t2
infixr 1 :~>

type family XKind (p :: Pass)


-- | Combines two 'LexInfo's
-- This assumes that the first one comes before the second
-- and that they are both part of the same file.
mergeLexInfo :: LexInfo -> LexInfo -> LexInfo
mergeLexInfo (LexInfo {startPos, file}) (LexInfo {endPos}) = LexInfo {startPos, endPos, file}

data StructDef p = StructDef {
        _structName :: Name p
    ,   _structParams :: [TVar p]
    ,   _structFields :: [(UnqualifiedName, Type p)]
    }

structKind :: (IsKind (XKind p), Profunctor pf, Contravariant f) => Optic' pf f (StructDef p) (XKind p)
structKind = to \sd -> foldr (\(MkTVar _ k) r -> k `kFun` r) kStar (_structParams sd)

structType :: (IsKind (XKind p), Profunctor pf, Contravariant f) => Optic' pf f (StructDef p) (Type p)
structType = to \sd -> TCon (_structName sd) (view structKind sd) 

type instance InstanceRequirements (StructDef p) = [Name p, TVar p, Type p]

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
    show (KFun k1 k2) = "(" <> show k1 <> " -> " <> show k2 <> ")"

class TyLit n where
    tyIntT  :: n
    tyBoolT :: n
    tyUnitT :: n 
    tyFunT  :: n
    
instance TyLit QualifiedName where
    tyIntT  = internalQName "Int"
    tyBoolT = internalQName "Bool"
    tyUnitT = internalQName "Unit"
    tyFunT  = internalQName "->"
instance TyLit Text where
    tyIntT = "Int"
    tyBoolT = "Bool"
    tyUnitT = "Unit"
    tyFunT  = "->"

intT, boolT, unitT :: (IsKind (XKind p), TyLit (Name p)) => Type p
intT  = TCon tyIntT (fromKind KStar)
boolT = TCon tyBoolT (fromKind KStar)
unitT = TCon tyUnitT (fromKind KStar)

class IsKind t where 
    kFun :: t -> t -> t
    kStar :: t
    kStar = fromKind KStar
    kConstraint :: t
    kConstraint = fromKind KConstraint
    fromKind :: Kind -> t
    fromKind KStar = kStar
    fromKind (KFun k1 k2) = kFun (fromKind k1) (fromKind k2)
    fromKind (KConstraint) = kConstraint
    {-# MINIMAL kFun, (fromKind | (kStar, kConstraint)) #-}
instance IsKind () where 
    fromKind _ = ()
    kFun _ _   = ()
instance IsKind Kind where 
    fromKind = id
    kFun = KFun 

class HasKind t where
    kind :: t -> Either (Kind, Kind) Kind

instance (XKind p ~ Kind) => HasKind (TVar p) where
    kind (MkTVar _ k) = Right k

instance ((XKind p) ~ Kind) => HasKind (Type p) where
    kind = \case
        TVar v -> kind v
        TSkol v -> kind v
        TCon _ k -> pure k
        TApp t1 t2 -> bisequence (kind t1, kind t2) >>= \case
            (KFun kp kr, ka)
                | kp == ka -> pure kr
            (k1, k2) -> Left (k1, k2)
        TForall _ t -> kind t
        TConstraint _ t -> kind t 
    
instance (HasKind (Type p)) => HasKind (Constraint p) where
    kind _ = pure kConstraint 

-- | A class used to coerce types between compatible passes (i.e. passes where the types are equivalent).
-- Instances for this *should not* be written by hand, but generated by TemplateHaskell, since we have to ensure
-- that all constraints are included
class CoercePass a b
        
coercePass :: (CoercePass t1 t2) => t1 -> t2
coercePass = Unsafe.Coerce.unsafeCoerce

instance {-# INCOHERENT #-} (Coercible a b) => CoercePass a b

instance {-# INCOHERENT #-} (CoercePass k1 k2, CoercePass v1 v2, Ord k2) => CoercePass (Map k1 v1) (Map k2 v2)

instance {-#INCOHERENT#-} (CoercePass a b) => CoercePass [a] [b]

instance {-# INCOHERENT #-} (CoercePass a a', CoercePass b b') => CoercePass (a, b) (a', b')
        
instance {-# INCOHERENT #-} (CoercePass a a', CoercePass b b', CoercePass c c') => CoercePass (a, b, c) (a', b', c')

deriveCoercePass ''Module
deriveCoercePass ''Statement
deriveCoercePass ''Expr
deriveCoercePass ''CaseBranch
deriveCoercePass ''Pattern
deriveCoercePass ''Decl
deriveCoercePass ''Type
deriveCoercePass ''Constraint
deriveCoercePass ''TVar
deriveCoercePass ''StructDef

-- TODO: Cannot be generated with TH right now, since it depends on f
instance {-# INCOHERENT #-} (CoercePass (Name p1) (Name p2), CoercePass (Expr p1) (Expr p2)) => CoercePass (OperatorGroup p1 f) (OperatorGroup p2 f) where

class HasLexInfo t where
    getLexInfo :: t -> LexInfo

instance HasLexInfo (Expr p) where
    getLexInfo = \case
        FCall _ li _ _           -> li
        IntLit _ li _            -> li
        UnitLit li               -> li
        If _ li _ _ _            -> li
        Let _ li _ _             -> li
        Var _ li _               -> li
        Ascription _ li _ _      -> li
        VariantConstr _ li _     -> li
        Case _ li _ _            -> li
        StructConstruct _ li _ _ -> li
        StructAccess _ li _ _    -> li
        Lambda _ li _ _          -> li
        ExprX _ li               -> li
        

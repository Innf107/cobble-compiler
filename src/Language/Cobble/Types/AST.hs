{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Cobble.Types.AST where

import Language.Cobble.Prelude
import Language.Cobble.Util.Convert
import Language.Cobble.Util.TypeUtils

import Language.Cobble.Shared

import Language.Cobble.MCAsm.Types (Objective)

import Data.Data
import Data.Generics.Uniplate.Data

import GHC.Show qualified as S

import qualified Unsafe.Coerce 

type family Name (p :: Pass)

type family InstanceRequirements t :: [HSType]

-- Top level module.
data Module (p :: Pass) = Module
    { xModule :: (XModule p)
    , moduleName :: (Name p)
    , moduleStatements :: [Statement p]
    }

type instance InstanceRequirements (Module p) = [XModule p, Name p, Statement p]

deriving instance (AllC Show             (InstanceRequirements (Module p))) => Show    (Module p)
deriving instance (AllC Eq               (InstanceRequirements (Module p))) => Eq      (Module p)
deriving instance (Typeable p, AllC Data (InstanceRequirements (Module p))) => Data    (Module p)
deriving instance                                                              Generic (Module p)

type family XModule (p :: Pass)

-- | The signature of a module contains
-- everything that it exports
-- (Variables, Functions, Types, etc.)
data ModSig = ModSig {
    exportedVars :: Map QualifiedName (Type 'Codegen)
,   exportedTypes :: Map QualifiedName (Kind, TypeVariant)
} deriving (Generic, Typeable) -- Instances for @Eq@ and @Data@ are defined in Language.Cobble.Types.AST.Codegen

data TypeVariant = RecordType [(QualifiedName, Type 'Codegen)]
                 | BuiltInType
                 deriving (Generic, Typeable)

type Dependencies = Map QualifiedName ModSig

instance Semigroup ModSig where ModSig vs ts <> ModSig vs' ts' = ModSig (vs <> vs') (ts <> ts')
instance Monoid ModSig where mempty = ModSig mempty mempty

-- | A data kind representing the state of the AST at a certain Compiler pass.
data Pass = SolveModules
          | QualifyNames
          | SemAnalysis
          | Typecheck
          | Codegen


data Statement (p :: Pass) =
      Def        (XDef p)       LexInfo (Decl p) (Type p)
    | Import     (XImport p)    LexInfo (Name p) -- TODO: qualified? exposing?
    | DefStruct  (XDefStruct p) LexInfo (Name p) [(Name p, Type p)]
    | StatementX (XStatement p) LexInfo

type instance InstanceRequirements (Statement p) = [XDef p, XImport p, XDefStruct p, XStatement p, Name p, Type p, Decl p]

deriving instance (AllC Show             (InstanceRequirements (Statement p))) => Show    (Statement p)
deriving instance (AllC Eq               (InstanceRequirements (Statement p))) => Eq      (Statement p)
deriving instance (Typeable p, AllC Data (InstanceRequirements (Statement p))) => Data    (Statement p)
deriving instance                                                                 Generic (Statement p)

type family XDef            (p :: Pass)
type family XParam          (p :: Pass)
type family XImport         (p :: Pass)
type family XDefStruct      (p :: Pass)
type family XStatement      (p :: Pass)


data Decl (p :: Pass) = Decl (XDecl p) (Name p) (XParam p) (Expr p)

type instance InstanceRequirements (Decl p) = [XDecl p, Name p, XParam p, Expr p]
deriving instance (AllC Show             (InstanceRequirements (Decl p))) => Show    (Decl p)
deriving instance (AllC Eq               (InstanceRequirements (Decl p))) => Eq      (Decl p)
deriving instance (Typeable p, AllC Data (InstanceRequirements (Decl p))) => Data    (Decl p)
deriving instance                                                            Generic (Decl p)

type family XDecl (p :: Pass)

data Expr (p :: Pass) =
      FCall           (XFCall p) LexInfo (Expr p) (NonEmpty (Expr p))
    | IntLit          (XIntLit p) LexInfo Int
    | UnitLit         LexInfo
    | If              (XIf p) LexInfo (Expr p) (Expr p) (Expr p)
    | Let             (XLet p) LexInfo (Decl p) (Expr p)
    | Var             (XVar p) LexInfo (Name p)
    | StructConstruct (XStructConstruct p) LexInfo (Name p) [(Name p, Expr p)]
    | ExprX           (XExpr p) LexInfo

type instance InstanceRequirements (Expr p) = [XFCall p, XIntLit p, XIf p, XLet p, Decl p, XVar p, Name p, XStructConstruct p, XExpr p]

deriving instance (AllC Show             (InstanceRequirements (Expr p))) => Show    (Expr p)
deriving instance (AllC Eq               (InstanceRequirements (Expr p))) => Eq      (Expr p)
deriving instance (Typeable p, AllC Data (InstanceRequirements (Expr p))) => Data    (Expr p)
deriving instance                                                            Generic (Expr p)

type family XFCall           (p :: Pass)
type family XIntLit          (p :: Pass)
type family XIf              (p :: Pass)
type family XLet             (p :: Pass)
type family XVar             (p :: Pass)
type family XStructConstruct (p :: Pass)
type family XExpr            (p :: Pass)

data Kind = KStar | KFun Kind Kind deriving (Eq, Generic, Data, Typeable)

infixr 5 `KFun`

data Type (p :: Pass) = TCon (Name p) (XKind p)
                      | TApp (Type p) (Type p)
                      | TVar (Name p) (XKind p)

type instance InstanceRequirements (Type p) = [Name p, XKind p]

deriving instance (AllC Show             (InstanceRequirements (Type p))) => Show    (Type p)
deriving instance (AllC Eq               (InstanceRequirements (Type p))) => Eq      (Type p)
deriving instance (Typeable p, AllC Data (InstanceRequirements (Type p))) => Data    (Type p)
deriving instance                                                            Generic (Type p)

(-:>) :: (TyLit (Name p), IsKind (XKind p)) => Type p -> Type p -> Type p
t1 -:> t2 = TApp (TApp (TCon tyFunT (kFun kStar (kFun kStar kStar))) t1) t2
infixr 5 -:>

pattern (:->) :: (Name p ~ QualifiedName, Eq (Name p), XKind p ~ Kind) => Type p -> Type p -> Type p
pattern (:->) t1 t2 = TApp (TApp (TCon "prims.->" (KFun KStar (KFun KStar KStar))) t1) t2

pattern (:~>) :: (Name p ~ Text, Eq (Name p), XKind p ~ Kind) => Type p -> Type p -> Type p
pattern (:~>) t1 t2 = TApp (TApp (TCon "->" (KFun KStar (KFun KStar KStar))) t1) t2


type family XKind (p :: Pass)

type FileName = Text


data LexInfo = LexInfo {
      startPos :: SourcePos
    , endPos :: SourcePos
    , file :: FileName
    } deriving (Show, Eq, Data, Typeable)

data SourcePos = SourcePos {
      line :: Int
    , column :: Int
    } deriving (Show, Eq, Ord, Data, Typeable)

-- | Combines two 'LexInfo's
-- This assumes that the first one comes before the second
-- and that they are both part of the same file.
mergeLexInfo :: LexInfo -> LexInfo -> LexInfo
mergeLexInfo (LexInfo {startPos, file}) (LexInfo {endPos}) = LexInfo {startPos, endPos, file}

data StructDef p = StructDef {
        _structName :: Name p
    ,   _structFields :: [(Name p, Type p)]
    }

type instance InstanceRequirements (StructDef p) = [Name p, Type p]

deriving instance (AllC Show             (InstanceRequirements (StructDef p))) => Show    (StructDef p)
deriving instance (AllC Eq               (InstanceRequirements (StructDef p))) => Eq      (StructDef p)
deriving instance (Typeable p, AllC Data (InstanceRequirements (StructDef p))) => Data    (StructDef p)
deriving instance                                                                 Generic (StructDef p)

instance (Name p1 ~ Name p2, XKind p1 ~ XKind p2) => Convert (Type p1) (Type p2) where
    conv = \case
        TCon n k   -> TCon n k
        TApp t1 t2 -> TApp (conv t1) (conv t2)
        TVar n k   -> TVar n k

instance S.Show Kind where
    show KStar = "*"
    show (KFun k1 k2) = "(" <> show k1 <> " -> " <> show k2 <> ")"

class TyLit n where
    tyIntT  :: n
    tyBoolT :: n
    tyUnitT :: n 
    tyFunT  :: n
    
instance TyLit QualifiedName where
    tyIntT  = "prims.Int"
    tyBoolT = "prims.Bool"
    tyUnitT = "prims.Unit"
    tyFunT  = "prims.->"
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
    fromKind :: Kind -> t
    fromKind KStar = kStar
    fromKind (KFun k1 k2) = kFun (fromKind k1) (fromKind k2)
    {-# MINIMAL kFun, (fromKind | kStar) #-}
instance IsKind () where 
    fromKind _ = ()
    kFun _ _   = ()
instance IsKind Kind where 
    fromKind = id
    kFun = KFun 

kind :: ((XKind p) ~ Kind) => Type p -> Either (Kind, Kind) Kind
kind = \case
    TVar _ k -> pure k
    TCon _ k -> pure k
    TApp t1 t2 -> bisequence (kind t1, kind t2) >>= \case
        (KFun kp kr, ka)
            | kp == ka -> pure kr
        (k1, k2) -> Left (k1, k2) 
    
      
class CoercePass t1 t2 p1 p2 | t1 -> p1, t2 -> p2 where
    _coercePass :: t1 -> t2
        
{-# NOINLINE coercePass #-}
coercePass :: (CoercePass t1 t2 p1 p2) => t1 -> t2
coercePass = _coercePass
{-# RULES "coercePass/coercePass" forall x. coercePass x = Unsafe.Coerce.unsafeCoerce x#-}

instance {-#OVERLAPPABLE#-} (Coercible (t p1) (t p2)) => CoercePass (t p1) (t p2) p1 p2 where
    _coercePass = coerce

data IgnoreExt (p :: Pass) = IgnoreExt deriving (Show, Eq)

instance CoercePass (IgnoreExt p1) (IgnoreExt p2) p1 p2 where
    _coercePass = coerce

type TypeCoercible p1 p2 = (Name p1 ~ Name p2
                           , XKind p1 ~ XKind p2)  

instance TypeCoercible p1 p2 => CoercePass (Type p1) (Type p2) p1 p2 where
    _coercePass = \case
        TCon n k -> TCon n k
        TApp t1 t2 -> TApp (coercePass t1) (coercePass t2)
        TVar n k -> TVar n k


type ExprCoercible p1 p2 = ( CoercePass (XStructConstruct p1) (XStructConstruct p2) p1 p2
                         , XFCall p1           ~ XFCall   p2
                         , XIntLit  p1         ~ XIntLit  p2
                         , Name     p1         ~ Name     p2
                         , XIf      p1         ~ XIf      p2
                         , XLet     p1         ~ XLet     p2
                         , XVar     p1         ~ XVar     p2
                         , XExpr    p1         ~ XExpr    p2
                         )
-- Ugly hack :(
--newtype XStructConstructL p = XStructConstructL (XStructConstruct p)
--instance (XStructConstruct p1 ~ StructDef p1, XStructConstruct p2 ~ StructDef p2, CoercePass StructDef p1 p2)
--    => CoercePass XStructConstructL p1 p2 where
--    _coercePass (XStructConstructL x) = XStructConstructL (coercePass x)


instance (ExprCoercible p1 p2, DeclCoercible p1 p2) => CoercePass (Expr p1) (Expr p2) p1 p2 where
    _coercePass = \case
        FCall x l f as           -> FCall x l (coercePass f) (fmap coercePass as)
        IntLit x l i             -> IntLit x l i
        UnitLit l                -> UnitLit l
        If x l c th el           -> If x l (coercePass c) (coercePass th) (coercePass el)
        Let x l d b              -> Let x l (coercePass d) (coercePass b)
        Var x l v                -> Var x l v
        StructConstruct x l c fs -> StructConstruct (coercePass  x) l c (map (second coercePass) fs)
        ExprX x l                -> ExprX x l

type StructDefCoercible p1 p2 = (Name p1 ~ Name p2, TypeCoercible p1 p2)

instance StructDefCoercible p1 p2 => CoercePass (StructDef p1) (StructDef p2) p1 p2 where
    _coercePass (StructDef n fs) = StructDef n (map (second coercePass) fs)

type DeclCoercible p1 p2 = (ExprCoercible p1 p2, XParam p1 ~ XParam p2, XKind p1 ~ XKind p2, XDecl p1 ~ XDecl p2)
        
instance (DeclCoercible p1 p2) => CoercePass (Decl p1) (Decl p2) p1 p2 where
  _coercePass (Decl x f ps e) = Decl x f ps (coercePass e)
        
type StatementCoercible p1 p2 = ( ExprCoercible p1 p2
                                , TypeCoercible p1 p2
                                , DeclCoercible p1 p2
                                , XDef       p1 ~ XDef    p2
                                , XParam     p1 ~ XParam  p2
                                , XImport    p1 ~ XImport p2
                                , XDefStruct p1 ~ XDefStruct p2
                                , XStatement p1 ~ XStatement p2
                                )
        
instance (StatementCoercible p1 p2) => CoercePass (Statement p1) (Statement p2) p1 p2 where
    _coercePass = \case
        Def x l d t -> Def x l (coercePass d) (coercePass t)
        Import x l n -> Import x l n
        DefStruct x l n fs -> DefStruct x l n (map (second coercePass) fs)
        StatementX x l -> StatementX x l
        
type ModuleCoercible p1 p2 = ( StatementCoercible p1 p2
                             , XModule p1 ~ XModule p2
                             )
instance (ModuleCoercible p1 p2, StatementCoercible p1 p2) => CoercePass (Module p1) (Module p2) p1 p2 where
    _coercePass (Module x n sts) = Module x n (map coercePass sts) 


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
        StructConstruct _ li _ _ -> li
        ExprX _ li               -> li
        


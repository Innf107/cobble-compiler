{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Cobble.Types.AST where

import Language.Cobble.Prelude
import Language.Cobble.Util.Convert

import Language.Cobble.Shared

import Language.Cobble.Types.TH

import Language.Cobble.MCAsm.Types (Objective)

import Data.Data
import Data.Generics.Uniplate.Data

import GHC.Show qualified as S

import qualified Unsafe.Coerce 

type family Name (p :: Pass)

-- Top level module.
data Module (p :: Pass) = Module
    { xModule :: (XModule p)
    , moduleName :: (Name p)
    , moduleStatements :: [Statement p]
    }

type family XModule (p :: Pass)

-- | The signature of a module contains
-- everything that it exports
-- (Variables, Functions, Types, etc.)
data ModSig = ModSig {
    exportedVars :: Map QualifiedName (Type 'Codegen)
,   exportedFunctions :: Map (QualifiedName) ([(QualifiedName, Type 'Codegen)], Maybe (Type 'Codegen))
,   exportedStructs :: Map (QualifiedName) [(QualifiedName, Type 'Codegen)]
} deriving (Generic, Typeable) -- Instances for @Eq@ and @Data@ are defined in Language.Cobble.Types.AST.Codegen

type Dependencies = Map QualifiedName ModSig

instance Semigroup ModSig where ModSig vs fs ts <> ModSig vs' fs' ts' = ModSig (vs <> vs') (fs <> fs') (ts <> ts')
instance Monoid ModSig where mempty = ModSig mempty mempty mempty

-- | A data kind representing the state of the AST at a certain Compiler pass.
data Pass = SolveModules
          | QualifyNames
          | Typecheck
          | Codegen


data Statement (p :: Pass) =
      Def        (XDef p)       LexInfo (Name p) [Name p] (Expr p) (Type p)

    | Import     (XImport p)    LexInfo (Name p) -- TODO: qualified? exposing?

    | DefStruct  (XDefStruct p) LexInfo (Name p) [(Name p, Type p)]
    | StatementX (XStatement p) LexInfo

data LogSegment p = LogText Text
                  | LogVar (Name p)

deriving instance (Typeable (Name p))         => Typeable (LogSegment p)
deriving instance (Typeable p, Data (Name p)) => Data (LogSegment p)
deriving instance Generic (LogSegment p)
deriving instance Show (Name p) => Show (LogSegment p)
deriving instance Eq (Name p) => Eq (LogSegment p)

instance (Name p1 ~ Name p2) => CoercePass LogSegment p1 p2 where
    _coercePass (LogText t) = LogText t
    _coercePass (LogVar n) = LogVar n

type family XDef            (p :: Pass)
type family XImport         (p :: Pass)
type family XDefStruct      (p :: Pass)
type family XStatement      (p :: Pass)


data Expr (p :: Pass) =
      FCall (XFCall p) LexInfo (Expr p) (NonEmpty (Expr p))
    | IntLit (XIntLit p) LexInfo Int
          --  | FloatLit Double Text TODO: Needs Standard Library (Postfixes?)
    | UnitLit LexInfo --TODO: Replace with variable in 'base'
    | BoolLit (XBoolLit p) LexInfo Bool
    | If    (XIf p) LexInfo (Expr p) (Expr p) (Expr p)
    | Let   (XLet p) LexInfo (Name p) (Expr p) (Expr p)
    | Var   (XVar p) LexInfo (Name p)
    | ExprX (XExpr p) LexInfo

type family XFCall   (p :: Pass)
type family XIntLit  (p :: Pass)
type family XBoolLit (p :: Pass)
type family XIf      (p :: Pass)
type family XLet     (p :: Pass)
type family XVar     (p :: Pass)
type family XExpr    (p :: Pass)

data Kind = KStar | KFun Kind Kind deriving (Eq, Generic, Data, Typeable)


data Type (p :: Pass) = TCon (Name p) (XKind p)
                      | TApp (Type p) (Type p)
                      | TVar (Name p) (XKind p)

(-:>) :: (IsString (Name p), IsKind (XKind p)) => Type p -> Type p -> Type p
t1 -:> t2 = TApp (TApp (TCon "->" (kFun kStar (kFun kStar kStar))) t1) t2 
infixr 5 -:>

pattern (:->) :: (IsString (Name p), Eq (Name p), XKind p ~ Kind) => Type p -> Type p -> Type p
pattern (:->) t1 t2 = TApp (TApp (TCon "->" (KFun KStar (KFun KStar KStar))) t1) t2

type family XKind (p :: Pass)

type FileName = Text


data LexInfo = LexInfo {
      line :: Int
    , column :: Int
    , file :: FileName
    } deriving (Show, Eq, Data, Typeable)

instance (Name p1 ~ Name p2, XKind p1 ~ XKind p2) => Convert (Type p1) (Type p2) where
    conv = \case
        TCon n k   -> TCon n k
        TApp t1 t2 -> TApp (conv t1) (conv t2)
        TVar n k   -> TVar n k

instance S.Show Kind where
    show KStar = "*"
    show (KFun k1 k2) = "(" <> show k1 <> " -> " <> show k2 <> ")"

class TyLit n where
    tyIntT :: n
    tyBoolT :: n
    tyUnitT :: n 
    
instance TyLit QualifiedName where
    tyIntT = "prims.Int"
    tyBoolT = "prims.Bool"
    tyUnitT = "prims.Unit"
instance TyLit Text where
    tyIntT = "Int"
    tyBoolT = "Bool"
    tyUnitT = "Unit"

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
    
      
class CoercePass t p1 p2 where
    _coercePass :: t p1 -> t p2
        
{-#NOINLINE coercePass#-}
coercePass :: (CoercePass t p1 p2) => t p1 -> t p2
coercePass = _coercePass
{-# RULES "coercePass/coercePass" forall x. coercePass x = Unsafe.Coerce.unsafeCoerce x#-}
        
type TypeCoercible p1 p2 = (Name p1 ~ Name p2
                           , XKind p1 ~ XKind p2)  

instance TypeCoercible p1 p2 => CoercePass Type p1 p2 where
    _coercePass = \case
        TCon n k -> TCon n k
        TApp t1 t2 -> TApp (coercePass t1) (coercePass t2)
        TVar n k -> TVar n k

type ExprCoercible p1 p2 = ( XFCall p1   ~ XFCall   p2
                         , XIntLit  p1   ~ XIntLit  p2
                         , Name     p1   ~ Name     p2
                         , XBoolLit p1   ~ XBoolLit p2
                         , XIf      p1   ~ XIf      p2
                         , XLet     p1   ~ XLet     p2
                         , XVar     p1   ~ XVar     p2
                         , XExpr    p1   ~ XExpr    p2
                         )

instance (ExprCoercible p1 p2) => CoercePass Expr p1 p2 where
    _coercePass = \case
        FCall x l f as -> FCall x l (coercePass f) (fmap coercePass as)
        IntLit x l i   -> IntLit x l i
        BoolLit x l b  -> BoolLit x l b
        UnitLit l      -> UnitLit l
        If x l c th el -> If x l (coercePass c) (coercePass th) (coercePass el)
        Let x l v e b  -> Let x l v (coercePass e) (coercePass b)
        Var x l v      -> Var x l v
        ExprX x l      -> ExprX x l
        
type StatementCoercible p1 p2 = ( ExprCoercible p1 p2
                                , TypeCoercible p1 p2
                                , XDef       p1 ~ XDef    p2 
                                , XImport    p1 ~ XImport p2
                                , XDefStruct p1 ~ XDefStruct p2
                                , XStatement p1 ~ XStatement p2
                                )
        
instance (StatementCoercible p1 p2) => CoercePass Statement p1 p2 where
    _coercePass = \case
        Def x l f ps e t -> Def x l f ps (coercePass e) (coercePass t)
        Import x l n -> Import x l n
        DefStruct x l n fs -> DefStruct x l n (map (second coercePass) fs)
        StatementX x l -> StatementX x l
        
type ModuleCoercible p1 p2 = ( StatementCoercible p1 p2
                             , XModule p1 ~ XModule p2
                             )
instance (ModuleCoercible p1 p2, StatementCoercible p1 p2) => CoercePass Module p1 p2 where
    _coercePass (Module x n sts) = Module x n (map coercePass sts) 


class HasLexInfo t where
    getLexInfo :: t -> LexInfo

instance HasLexInfo (Expr p) where
    getLexInfo = \case
        FCall _ li _ _      -> li
        IntLit _ li _       -> li
        BoolLit _ li _      -> li
        UnitLit li          -> li
        If _ li _ _ _       -> li
        Let _ li _ _ _      -> li
        Var _ li _          -> li
        ExprX _ li          -> li
        


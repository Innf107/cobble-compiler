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
      CallFun (XCallFun p) LexInfo (Name p) [Expr p]
    | DefVoid (XDefVoid p) LexInfo (Name p) [(Name p, Type p)] [Statement p]
    | DefFun  (XDefFun p)  LexInfo (Name p) [(Name p, Type p)] [Statement p] (Expr p) (Type p)
    | Import  (XImport p)  LexInfo (Name p) -- TODO: qualified? exposing?
--                                                          ^ last expr
    | Decl (XDecl p) LexInfo (Name p) (Maybe (Type p)) (Expr p)
    | Assign (XAssign p) LexInfo (Name p) (Expr p)
    | IfS (XIfS p) LexInfo (Expr p) [Statement p] (Maybe [Statement p])
    | While (XWhile p) LexInfo (Expr p) [Statement p]
    | DefStruct (XDefStruct p) LexInfo (Name p) [(Name p, Type p)]
    -- | /Temporary/ Statement until ConstStrs are implemented.
    | SetScoreboard (XSetScoreboard p) LexInfo Objective Text (Expr p)
    -- | /Temporary/ Statement until ConstStrs are implemented. Does not need extensions.
    | LogS LexInfo [LogSegment p]
--                                                       ^player
    | StatementX (XStatement p) LexInfo

data LogSegment p = LogText Text
                  | LogVar (Name p)

deriving instance (Typeable (Name p))         => Typeable (LogSegment p)
deriving instance (Typeable p, Data (Name p)) => Data (LogSegment p)
deriving instance Generic (LogSegment p)
deriving instance Show (Name p) => Show (LogSegment p)
deriving instance Eq (Name p) => Eq (LogSegment p)

instance (Name p1 ~ Name p2) => CoercePass LogSegment p1 p2 where
    coercePass (LogText t) = LogText t
    coercePass (LogVar n) = LogVar n

type family XCallFun        (p :: Pass)
type family XDefVoid        (p :: Pass)
type family XDefFun         (p :: Pass)
type family XImport         (p :: Pass)
type family XDecl           (p :: Pass)
type family XAssign         (p :: Pass)
type family XIfS            (p :: Pass)
type family XWhile          (p :: Pass)
type family XDefStruct      (p :: Pass)
type family XSetScoreboard  (p :: Pass)
type family XStatement      (p :: Pass)


data Expr (p :: Pass) =
      FCall (XFCall p) LexInfo (Name p) [Expr p]
    | IntLit (XIntLit p) LexInfo Int
          --  | FloatLit Double Text TODO: Needs Standard Library (Postfixes?)
    | BoolLit (XBoolLit p) LexInfo Bool
    | IfE (XIfE p) LexInfo (Expr p) (Expr p) (Expr p)
    | Var (XVar p) LexInfo (Name p)
    | ExprX (XExpr p) LexInfo

type family XFCall (p :: Pass)
type family XIntLit (p :: Pass)
type family XBoolLit (p :: Pass)
type family XIfE (p :: Pass)
type family XVar (p :: Pass)
type family XExpr (p :: Pass)

data Kind = KStar | KFun Kind Kind deriving (Eq, Generic, Data, Typeable)


data Type (p :: Pass) = TCon (Name p) (XKind p)
                      | TApp (Type p) (Type p)
                      | TVar (Name p) (XKind p)

(-:>) :: (IsString (Name p), IsKind (XKind p)) => Type p -> Type p -> Type p
t1 -:> t2 = TApp (TApp (TCon "->" (kFun kStar (kFun kStar kStar))) t1) t2 
infixr 5 -:>

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
    
instance TyLit QualifiedName where
    tyIntT = "prims.Int"
    tyBoolT = "prims.Bool"
instance TyLit Text where
    tyIntT = "Int"
    tyBoolT = "Bool"

intT, boolT :: (IsKind (XKind p), TyLit (Name p)) => Type p
intT  = TCon tyIntT (fromKind KStar)
boolT = TCon tyBoolT (fromKind KStar)

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
    coercePass :: t p1 -> t p2
        
type TypeCoercible p1 p2 = (Name p1 ~ Name p2
                           , XKind p1 ~ XKind p2)  

instance TypeCoercible p1 p2 => CoercePass Type p1 p2 where
    coercePass = \case
        TCon n k -> TCon n k
        TApp t1 t2 -> TApp (coercePass t1) (coercePass t2)
        TVar n k -> TVar n k

type ExprCoercible p1 p2 = ( XFCall p1   ~ XFCall p2
                         , XIntLit p1  ~ XIntLit p2
                         , Name p1     ~ Name p2
                         , XBoolLit p1 ~ XBoolLit p2
                         , XIfE p1     ~ XIfE p2
                         , XVar p1     ~ XVar p2
                         , XExpr p1    ~ XExpr p2
                         )

instance (ExprCoercible p1 p2) => CoercePass Expr p1 p2 where
    coercePass = \case
        FCall x l n as -> FCall x l n (map coercePass as)
        IntLit x l i   -> IntLit x l i
        BoolLit x l b  -> BoolLit x l b
        IfE x l c t e  -> IfE x l (coercePass c) (coercePass t) (coercePass e)
        Var x l v      -> Var x l v
        ExprX x l      -> ExprX x l
        
type StatementCoercible p1 p2 = ( ExprCoercible p1 p2
                                , TypeCoercible p1 p2
                                , XCallFun p1 ~ XCallFun p2
                                , XDefVoid p1 ~ XDefVoid p2
                                , XDefFun p1 ~ XDefFun p2
                                , XImport p1 ~ XImport p2
                                , XDecl p1 ~ XDecl p2
                                , XAssign p1 ~ XAssign p2
                                , XIfS p1 ~ XIfS p2
                                , XWhile p1 ~ XWhile p2
                                , XDefStruct p1 ~ XDefStruct p2
                                , XSetScoreboard p1 ~ XSetScoreboard p2
                                , XStatement p1 ~ XStatement p2
                                )
        
instance (StatementCoercible p1 p2) => CoercePass Statement p1 p2 where
    coercePass = \case
        CallFun x l n as -> CallFun x l n (map coercePass as)
        DefVoid x l n ps b -> DefVoid x l n (map (second coercePass) ps) (map coercePass b)
        DefFun x l n ps b r rt -> DefFun x l n (map (second coercePass) ps) (map coercePass b) (coercePass r) (coercePass rt)
        Import x l n -> Import x l n
        Decl x l n mt e -> Decl x l n (fmap coercePass mt) (coercePass e)
        Assign x l n e -> Assign x l n (coercePass e)
        IfS x l c th el -> IfS x l (coercePass c) (map coercePass th) (map coercePass <$> el)
        While x l c b -> While x l (coercePass c) (map coercePass b)
        DefStruct x l n fs -> DefStruct x l n (map (second coercePass) fs)
        SetScoreboard x l o t e -> SetScoreboard x l o t (coercePass e)
        LogS l segs -> LogS l (map coercePass segs)
        StatementX x l -> StatementX x l
        
type ModuleCoercible p1 p2 = ( StatementCoercible p1 p2
                             , XModule p1 ~ XModule p2
                             )
instance (ModuleCoercible p1 p2, StatementCoercible p1 p2) => CoercePass Module p1 p2 where
    coercePass (Module x n sts) = Module x n (map coercePass sts) 


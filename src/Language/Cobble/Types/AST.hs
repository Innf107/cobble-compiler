{-# LANGUAGE TemplateHaskell#-}
module Language.Cobble.Types.AST where

import Language.Cobble.Prelude
import Language.Cobble.Util.Convert

import Language.Cobble.Shared

import Language.Cobble.Types.TH

import Language.Cobble.MCAsm.Types (Objective)

import GHC.Show qualified as S

type family Name (p :: Pass)

-- Top level module.
data Module (p :: Pass) = Module (XModule p) (Name p) [Statement p] --deriving (Show, Eq)

type family XModule (p :: Pass)

-- | A data kind representing the state of the AST at a certain Compiler pass.
data Pass = QualifyNames
          | Typecheck
          | Codegen
          deriving (Show, Eq)


data Statement (p :: Pass) =
      CallFun (XCallFun p) LexInfo (Name p) [Expr p]
    | DefVoid (XDefVoid p) LexInfo (Name p) [(Name p, Type p)] [Statement p]
    | DefFun  (XDefFun p)  LexInfo (Name p) [(Name p, Type p)] [Statement p] (Expr p) (Type p)
--                                                          ^ last expr
    | Decl (XDecl p) LexInfo (Name p) (Maybe (Type p)) (Expr p)
    | Assign (XAssign p) LexInfo (Name p) (Expr p)
    | While (XWhile p) LexInfo (Expr p) [Statement p]
    | DefStruct (XDefStruct p) LexInfo (Name p) [(Name p, Type p)]
    | SetScoreboard (XSetScoreboard p) LexInfo Objective Text (Expr p)
--                                                       ^player
    | StatementX (XStatement p) LexInfo


type family XCallFun (p :: Pass)
type family XDefVoid (p :: Pass)
type family XDefFun (p :: Pass)
type family XDecl (p :: Pass)
type family XAssign (p :: Pass)
type family XWhile (p :: Pass)
type family XDefStruct (p :: Pass)
type family XSetScoreboard (p :: Pass)
type family XStatement (p :: Pass)


data Expr (p :: Pass) =
      FCall (XFCall p) LexInfo (Name p) [Expr p]
    | IntLit (XIntLit p) LexInfo Int
          --  | FloatLit Double Text TODO: Needs Standard Library (Postfixes?)
    | BoolLit (XBoolLit p) LexInfo Bool
    | Var (XVar p) LexInfo (Name p)
    | ExprX (XExpr p) LexInfo

type family XFCall (p :: Pass)
type family XIntLit (p :: Pass)
type family XBoolLit (p :: Pass)
type family XVar (p :: Pass)
type family XExpr (p :: Pass)

data Kind = KStar | KFun Kind Kind deriving (Eq)


data Type (p :: Pass) = TCon (Name p) (XKind p)
                      | TApp (Type p) (Type p)
                      | TVar (Name p) (XKind p)

type family XKind (p :: Pass)

type FileName = Text


data LexInfo = LexInfo {
      line :: Int
    , column :: Int
    , file :: FileName
    } deriving (Show, Eq)

instance (Name p1 ~ Name p2, XKind p1 ~ XKind p2) => Convert (Type p1) (Type p2) where
    conv = \case
        TCon n k   -> TCon n k
        TApp t1 t2 -> TApp (conv t1) (conv t2)
        TVar n k   -> TVar n k

instance S.Show Kind where
    show KStar = "*"
    show (KFun k1 k2) = "(" <> show k1 <> ") -> (" <> show k2 <> ")"

class TyLit n where
    tyIntT :: n
    tyBoolT :: n
    
instance TyLit QualifiedName where
    tyIntT = "Prelude.Int"
    tyBoolT = "Prelude.Bool"
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
    
      

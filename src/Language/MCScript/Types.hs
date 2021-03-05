{-# LANGUAGE NoImplicitPrelude, DataKinds, TypeFamilies, StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE GADTs, RankNTypes, PatternSynonyms, TemplateHaskell#-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Language.MCScript.Types where

import Language.MCScript.Prelude

import Language.MCScript.Types.TH

type Name = Text

-- Top level module. 
data Module (t :: Pass) = Module Name [Statement t] --deriving (Show, Eq)

-- | A data kind representing Compiler passes.
data Pass = Unaltered
          | Typed
          deriving (Show, Eq)


data Statement (p :: Pass) =
      CallFun (XCallFun p) Name [Expr p]
    | DefVoid (XDefVoid p) Name [(Name, Type)] [Statement p]
    | DefFun (XDefFun p) Name [(Name, Type)] [Statement p] (Expr p) Type
--                                                          ^ last expr
    | Decl (XDecl p) Name (Maybe Type) (Expr p)
    | Assign (XAssign p) Name (Expr p)
    | While (XWhile p) (Expr p) [Statement p]
    | DefStruct (XDefStruct p) Name [(Name, Type)]
    | StatementX (XStatement p)

type family XCallFun (p :: Pass)
type family XDefVoid (p :: Pass)
type family XDefFun (p :: Pass)
type family XDecl (p :: Pass)
type family XAssign (p :: Pass)
type family XWhile (p :: Pass)
type family XDefStruct (p :: Pass)
type family XStatement (p :: Pass)


deriving instance Show (Statement 'Typed)
deriving instance Show (Statement 'Unaltered)
deriving instance Eq (Statement 'Typed)
deriving instance Eq (Statement 'Unaltered)


data Expr (p :: Pass) =
      FCall (XFCall p) Name [Expr p]
    | IntLit (XIntLit p) Int
          --  | FloatLit Double Text TODO: Needs Standard Library (Postfixes?)
    | BoolLit (XBoolLit p) Bool
    | Var (XVar p) Name

type family XFCall (p :: Pass)
type family XIntLit (p :: Pass)
type family XBoolLit (p :: Pass)
type family XVar (p :: Pass)

deriving instance Show (Expr 'Typed)
deriving instance Show (Expr 'Unaltered)
deriving instance Eq (Expr 'Typed)
deriving instance Eq (Expr 'Unaltered)


data Type = IntT | BoolT | EntityT | StructT Name deriving (Show, Eq)



type instance XCallFun 'Unaltered = Void
type instance XDefVoid 'Unaltered = Void
type instance XDefFun 'Unaltered = Void
type instance XDecl 'Unaltered = Void
type instance XAssign 'Unaltered = Void
type instance XWhile 'Unaltered = Void
type instance XDefStruct 'Unaltered = Void
type instance XStatement 'Unaltered = Void

type instance XFCall 'Unaltered = Void
type instance XIntLit 'Unaltered = Void
type instance XBoolLit 'Unaltered = Void
type instance XVar 'Unaltered = Void

makeSynonyms 'Unaltered ''Statement "U"

makeSynonyms 'Unaltered ''Expr "U"

makeSynonyms 'Typed ''Statement "T"

pattern FCallT :: Type -> Name -> [Expr 'Typed] -> Expr 'Typed
pattern FCallT t n ps <- FCall t n ps
    where
        FCallT t n ps = FCall t n ps

pattern IntLitT :: Int -> Expr 'Typed
pattern IntLitT i <- IntLit _ i
    where
        IntLitT i = IntLit void_ i

pattern BoolLitT :: Bool -> Expr 'Typed
pattern BoolLitT b <- BoolLit _ b
    where
        BoolLitT b = BoolLit void_ b

pattern VarT :: Type -> Name -> Expr 'Typed
pattern VarT t v <- Var t v
    where
        VarT t v = Var t v


type instance XCallFun 'Typed = Void -- TODO: Should this keep the return type?
type instance XDefVoid 'Typed = Void
type instance XDefFun 'Typed = Void
type instance XDecl 'Typed = Void
type instance XAssign 'Typed = Void
type instance XWhile 'Typed = Void
type instance XDefStruct 'Typed = Void
type instance XStatement 'Typed = Void

type instance XFCall 'Typed = Type
type instance XIntLit 'Typed = Void
type instance XBoolLit 'Typed = Void
type instance XVar 'Typed = Type

exprType :: Expr 'Typed -> Type
exprType = \case
    FCall t _ _ -> t
    IntLit _ _ -> IntT
    BoolLit _ _ -> BoolT
    Var t _ -> t



void_ :: Void
void_ = error "Attempt to evaluate void"

{- Postfixes:
1.23 -> 1.23f -> FloatLit 1.23 "f" -> mkFloat(123, -3)

postfix "f" mkFloat;

Float mkFloat (exponent: Int, mantissa: Int) {
    Float {
      exponent = exponent
    , mantissa = mantissa
    }
}

postfix "r" mkRational;

Rational mkRational (exponent: Int, mantissa: Int) {
    ...
}

-}

{- Floats:

1,5 = 15 * 10^-1 (As Library Struct)

Struct:
    struct Float {
      exponent: Int
    , mantissa: Int
    }

    // (x * 10^a) * (y * 10^b) = (x * y) * (10^a * 10^b) = (x * y) * (10^(a + b))
    Float mulFloat(Float x, Float y){
        normalize(Float {
          exponent=x.exponent + y.exponent
        , mantissa=x.mantissa * y.mantissa
        })
    }

    Float int2Float(Int x){
        normalize(Float {exponent=1, mantissa=x})
    }

    Int floor(Float x){
        if (x.exponent >= 0)
            x.mantissa * (10 ^ x.exponent)
        else
            0
    }

    // (x * 10^a) / (y * 10^b) = (x / y) * 10^(a - b)
    Float divFloat(Float x, Float y){

    }


-}



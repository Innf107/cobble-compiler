{-# LANGUAGE NoImplicitPrelude, DataKinds, TypeFamilies, StandaloneDeriving, FlexibleInstances #-}
module Language.MCScript.Types where

import Language.MCScript.Prelude

import qualified Data.Kind as K

type Name = Text

-- Top level module. 
data Module (t :: Typing) = Module Name [Statement t] --deriving (Show, Eq)

data Typing = Typed | Untyped

type family TypingData (t :: Typing) (a :: K.Type) where
    TypingData 'Typed a = (a, Type)
    TypingData 'Untyped a = a
    

data Statement (t::Typing) = CallVoid Name [(TypingData t (Expr t))]
               | CallFun Name [(TypingData t (Expr t))]
               | DefVoid Name [(Name, Type)] [Statement t]
               | DefFun Name [(Name, Type)] [Statement t] (TypingData t (Expr t)) Type
--                                                      ^ last expr
               | Decl Name Type (TypingData t (Expr t))
               | Assign Name (TypingData t (Expr t))
               | While (TypingData t (Expr t)) [Statement t]
              --  | Return Expr   -- potentially very hard to implement?
               | DefStruct Name [(Name, Type)]
               
deriving instance Show (Statement 'Typed)
deriving instance Show (Statement 'Untyped)
deriving instance Eq (Statement 'Typed)
deriving instance Eq (Statement 'Untyped)


data Expr (t :: Typing) = FCall Name [TypingData t (Expr t)]
          | IntLit Int
          --  | FloatLit Double Text TODO: Needs Standard Library (Postfixes?)
          | BoolLit Bool
          | Var Name

deriving instance Show (Expr 'Typed)
deriving instance Show (Expr 'Untyped)
deriving instance Eq (Expr 'Typed)
deriving instance Eq (Expr 'Untyped)


data Type = IntT | BoolT | EntityT | StructT Name [Type] deriving (Show, Eq)


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



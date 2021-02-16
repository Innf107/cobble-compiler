{-# LANGUAGE NoImplicitPrelude, DataKinds, TypeFamilies #-}
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
               --deriving (Show, Eq)

data Expr (t :: Typing) = FCall Name [TypingData t (Expr t)]
          | IntLit Int
          | FloatLit Double
          | BoolLit Bool
          | Var Name
          --deriving (Show, Eq)


data Type = IntT | FloatT | BoolT | EntityT | StructT Name [Type] deriving (Show, Eq)

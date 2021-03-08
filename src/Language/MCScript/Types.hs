{-# LANGUAGE NoImplicitPrelude, DataKinds, TypeFamilies, StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE GADTs, RankNTypes, PatternSynonyms, TemplateHaskell#-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Language.MCScript.Types where

import Language.MCScript.Prelude

import Language.MCScript.Types.TH

type Name = Text

-- Top level module. 
data Module (t :: Pass) = Module Name [Statement t] --deriving (Show, Eq)

-- | A data kind representing the state of the AST at a certain Compiler pass.
data Pass = UnexpandedMacros
          | Unaltered
          | Typed
          deriving (Show, Eq)


data Statement (p :: Pass) =
      CallFun (XCallFun p) LexInfo Name [Expr p]
    | DefVoid (XDefVoid p) LexInfo Name [(Name, TypeInfo p)] [Statement p]
    | DefFun  (XDefFun p) LexInfo Name [(Name, TypeInfo p)] [Statement p] (Expr p) (TypeInfo p)
    --  | DefMacro (XDefMacro p) LexInfo Name [()]
--                                                          ^ last expr
    | Decl  (XDecl p) LexInfo Name (Maybe (TypeInfo p)) (Expr p)
    | Assign (XAssign p) LexInfo Name (Expr p)
    | While (XWhile p) LexInfo (Expr p) [Statement p]
    | DefStruct (XDefStruct p) LexInfo Name [(Name, TypeInfo p)]
    | StatementX (XStatement p) LexInfo


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
deriving instance Show (Statement 'UnexpandedMacros)
deriving instance Eq (Statement 'Typed)
deriving instance Eq (Statement 'Unaltered)
deriving instance Eq (Statement 'UnexpandedMacros)


data Expr (p :: Pass) =
      FCall (XFCall p) LexInfo Name [Expr p]
    | IntLit (XIntLit p) LexInfo Int
          --  | FloatLit Double Text TODO: Needs Standard Library (Postfixes?)
    | BoolLit (XBoolLit p) LexInfo Bool
    | Var (XVar p) LexInfo Name
    | ExprX (XExpr p) LexInfo

type family XFCall (p :: Pass)
type family XIntLit (p :: Pass)
type family XBoolLit (p :: Pass)
type family XVar (p :: Pass)
type family XExpr (p :: Pass)

type family TypeInfo (p :: Pass)

deriving instance Show (Expr 'Typed)
deriving instance Show (Expr 'Unaltered)
deriving instance Show (Expr 'UnexpandedMacros)
deriving instance Eq (Expr 'Typed)
deriving instance Eq (Expr 'Unaltered)
deriving instance Eq (Expr 'UnexpandedMacros)

data Type = IntT | BoolT | EntityT | StructT Name deriving (Show, Eq)

type FileName = Text


data LexInfo = LexInfo {
      line :: Int
    , column :: Int
    , file :: FileName
    } deriving (Show, Eq)


type instance XCallFun 'Unaltered = ()
type instance XDefVoid 'Unaltered = ()
type instance XDefFun 'Unaltered = ()
type instance XDecl 'Unaltered = ()
type instance XAssign 'Unaltered = ()
type instance XWhile 'Unaltered = ()
type instance XDefStruct 'Unaltered = ()
type instance XStatement 'Unaltered = ()

type instance XFCall 'Unaltered = ()
type instance XIntLit 'Unaltered = ()
type instance XBoolLit 'Unaltered = ()
type instance XVar 'Unaltered = ()
type instance XExpr 'Unaltered = ()

type instance TypeInfo 'Unaltered = Type

makeSynonyms 'Unaltered ''Statement "U"

makeSynonyms 'Unaltered ''Expr "U"

makeSynonyms 'Typed ''Statement "T"

pattern FCallT :: Type -> LexInfo -> Name -> [Expr 'Typed] -> Expr 'Typed
pattern FCallT t l n ps <- FCall t l n ps
    where
        FCallT t l n ps = FCall t l n ps

pattern IntLitT :: LexInfo -> Int -> Expr 'Typed
pattern IntLitT l i <- IntLit _ l i
    where
        IntLitT l i = IntLit () l i

pattern BoolLitT :: LexInfo -> Bool -> Expr 'Typed
pattern BoolLitT l b <- BoolLit _ l b
    where
        BoolLitT l b = BoolLit () l b

pattern VarT :: Type -> LexInfo -> Name -> Expr 'Typed
pattern VarT t l v <- Var t l v
    where
        VarT t l v = Var t l v


type instance XCallFun 'Typed = () -- TODO: Should this keep the return type?
type instance XDefVoid 'Typed = ()
type instance XDefFun 'Typed = ()
type instance XDecl 'Typed = ()
type instance XAssign 'Typed = ()
type instance XWhile 'Typed = ()
type instance XDefStruct 'Typed = ()
type instance XStatement 'Typed = ()

type instance XFCall 'Typed = Type
type instance XIntLit 'Typed = ()
type instance XBoolLit 'Typed = ()
type instance XVar 'Typed = Type
type instance XExpr 'Typed = ()

type instance TypeInfo 'Typed = Type

exprType :: Expr 'Typed -> Type
exprType = \case
    FCall t _ _ _ -> t
    IntLit _ _ _ -> IntT
    BoolLit _ _ _ -> BoolT
    Var t _ _ -> t

type instance XCallFun 'UnexpandedMacros = ()
type instance XDefFun 'UnexpandedMacros = ()
type instance XDefVoid 'UnexpandedMacros = ()
type instance XDecl 'UnexpandedMacros = ()
type instance XAssign 'UnexpandedMacros = ()
type instance XWhile 'UnexpandedMacros = ()
type instance XDefStruct 'UnexpandedMacros = ()
type instance XStatement 'UnexpandedMacros = CallStatementMacroX

data CallStatementMacroX = CallStatementMacroX Name [MacroParam] deriving (Show, Eq)

data MacroParam = PStatement (Statement 'UnexpandedMacros) 
                | PExpr (Expr 'UnexpandedMacros) 
                | PStatements [Statement 'UnexpandedMacros]
                deriving (Show, Eq) 

pattern CallStatementMacro :: LexInfo -> Name -> [MacroParam] -> Statement 'UnexpandedMacros
pattern CallStatementMacro l n ps <- StatementX (CallStatementMacroX n ps) l
    where
        CallStatementMacro l n ps = StatementX (CallStatementMacroX n ps) l

type instance XFCall 'UnexpandedMacros = ()
type instance XIntLit 'UnexpandedMacros = ()
type instance XBoolLit 'UnexpandedMacros = ()
type instance XVar 'UnexpandedMacros = ()
type instance XExpr 'UnexpandedMacros = () -- TODO: Expression Macro?

type instance TypeInfo 'UnexpandedMacros = Name

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



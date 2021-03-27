{-#OPTIONS_GHC -Wno-orphans#-}
{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Types.AST.Codegen where

import Language.Cobble.Prelude

import Language.Cobble.Types.AST
import Language.Cobble.Types.TH
import Language.Cobble.Shared

deriving instance Show (Statement 'Codegen)
deriving instance Eq (Statement 'Codegen)

deriving instance Show (Expr 'Codegen)
deriving instance Eq (Expr 'Codegen)

deriving instance Show (Type 'Codegen)
deriving instance Eq (Type 'Codegen)

makeSynonyms 'Codegen ''Statement "T"

pattern FCallT :: Type 'Codegen -> LexInfo -> Name 'Codegen -> [Expr 'Codegen] -> Expr 'Codegen
pattern FCallT t l n ps <- FCall t l n ps
    where
        FCallT t l n ps = FCall t l n ps

pattern IntLitT :: LexInfo -> Int -> Expr 'Codegen
pattern IntLitT l i <- IntLit _ l i
    where
        IntLitT l i = IntLit () l i

pattern BoolLitT :: LexInfo -> Bool -> Expr 'Codegen
pattern BoolLitT l b <- BoolLit _ l b
    where
        BoolLitT l b = BoolLit () l b

pattern VarT :: Type 'Codegen -> LexInfo -> Name 'Codegen -> Expr 'Codegen
pattern VarT t l v <- Var t l v
    where
        VarT t l v = Var t l v


type instance XModule 'Codegen = ()

type instance XCallFun 'Codegen = () -- TODO: Should this keep the return type?
type instance XDefVoid 'Codegen = ()
type instance XDefFun 'Codegen = ()
type instance XDecl 'Codegen = ()
type instance XAssign 'Codegen = ()
type instance XWhile 'Codegen = ()
type instance XDefStruct 'Codegen = ()
type instance XSetScoreboard 'Codegen = ()
type instance XStatement 'Codegen = ()

type instance XFCall 'Codegen = Type 'Codegen
type instance XIntLit 'Codegen = ()
type instance XBoolLit 'Codegen = ()
type instance XVar 'Codegen = Type 'Codegen
type instance XExpr 'Codegen = ()

type instance Name 'Codegen = QualifiedName

type instance XKind 'Codegen = Kind


exprType :: Expr 'Codegen -> Type 'Codegen
exprType = \case
    FCall t _ _ _ -> t
    IntLit _ _ _ -> intT
    BoolLit _ _ _ -> boolT
    Var t _ _ -> t

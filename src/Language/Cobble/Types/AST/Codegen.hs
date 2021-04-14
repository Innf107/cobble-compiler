{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.Codegen where
  
import Data.Data
import Data.Generics.Uniplate.Data
  
import Language.Cobble.Prelude

import Language.Cobble.Types.AST
import Language.Cobble.Types.TH
import Language.Cobble.Shared

-- Defined here because the instances need
-- @Type 'Codegen@ to be defined
deriving instance Show ModSig
deriving instance Eq ModSig
deriving instance Data ModSig

deriving instance Show     (Module 'Codegen)
deriving instance Eq       (Module 'Codegen)
deriving instance Generic  (Module 'Codegen)
deriving instance Data     (Module 'Codegen)
deriving instance Typeable (Module 'Codegen)

deriving instance Show     (Statement 'Codegen)
deriving instance Eq       (Statement 'Codegen)
deriving instance Generic  (Statement 'Codegen)
deriving instance Data     (Statement 'Codegen)
deriving instance Typeable (Statement 'Codegen)

deriving instance Show     (Expr 'Codegen)
deriving instance Eq       (Expr 'Codegen)
deriving instance Generic  (Expr 'Codegen)
deriving instance Data     (Expr 'Codegen)
deriving instance Typeable (Expr 'Codegen)

deriving instance Show     (Type 'Codegen)
deriving instance Eq       (Type 'Codegen)
deriving instance Generic  (Type 'Codegen)
deriving instance Data     (Type 'Codegen)
deriving instance Typeable (Type 'Codegen)

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


type instance XModule 'Codegen = Map (Name 'Codegen) ModSig

type instance XCallFun 'Codegen = () -- TODO: Should this keep the return type?
type instance XDefVoid 'Codegen = ()
type instance XDefFun 'Codegen = ()
type instance XImport 'Codegen = ()
type instance XDecl 'Codegen = ()
type instance XAssign 'Codegen = ()
type instance XIfS 'Codegen = (QualifiedName, Int)
type instance XWhile 'Codegen = ()
type instance XDefStruct 'Codegen = ()
type instance XSetScoreboard 'Codegen = ()
type instance XStatement 'Codegen = Void

type instance XFCall 'Codegen = Type 'Codegen
type instance XIntLit 'Codegen = ()
type instance XBoolLit 'Codegen = ()
type instance XVar 'Codegen = Type 'Codegen
type instance XExpr 'Codegen = Void

type instance Name 'Codegen = QualifiedName

type instance XKind 'Codegen = Kind

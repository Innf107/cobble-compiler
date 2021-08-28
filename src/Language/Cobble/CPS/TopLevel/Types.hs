module Language.Cobble.CPS.TopLevel.Types where

import Language.Cobble.Prelude
import qualified Prelude as P (Show(..))
import qualified Data.Text as T
import Language.Cobble.Types.QualifiedName
import Language.Cobble.Codegen.Common
import Language.Cobble.Codegen.PrimOps

data TL = LetF QualifiedName QualifiedName [QualifiedName] TLC TL
        | LetC QualifiedName [QualifiedName] TLC TL
        | C TLC
        deriving (Eq, Generic, Data)

data TLC = Let QualifiedName TLExp TLC
         | App QualifiedName [QualifiedName]
         | If QualifiedName TLC TLC
         deriving (Eq, Generic, Data)

data TLExp = IntLit Int
           | Var QualifiedName
           | Halt
           | Tuple [QualifiedName]
           | Select Int QualifiedName
           | PrimOp PrimOp [QualifiedName]
           deriving (Eq, Generic, Data)

instance Show TL where
    show = \case
      LetF f k xs e b -> "letf " <> show f <> " " <> show k <> " " <> intercalate " " (map show xs) <> " = (" <> show e <> ")\nin\n" <> show b
      LetC f xs e b -> "letc " <> show f <> " " <> intercalate " " (map show xs) <> " = (" <> show e <> ")\nin\n" <> show b 
      C tlc -> "    " <> show tlc 


instance Show TLC where
    show = \case
        Let x e b -> "let " <> show x <> " = " <> show e <> " in\n    " <> show b 
        App f xs -> show f <> " " <> intercalate " " (map show xs)
        If c th el -> "if " <> show c <> " then " <> show th <> " else " <> show el

instance Show TLExp where
    show = \case
        IntLit n    -> show n
        Var x       -> show x
        Halt        -> "halt"
        Tuple xs    -> "(" <> intercalate ", " (map show xs) <> ")"
        Select i x  -> show x <> "#" <> show i
        PrimOp p xs -> "(__" <> show p <> "__[" <> intercalate ", " (map show xs) <> "])"

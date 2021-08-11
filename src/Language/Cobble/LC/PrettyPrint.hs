module Language.Cobble.LC.PrettyPrint where

import Language.Cobble.Prelude
import Language.Cobble.LC.Types

import qualified Data.Text as T

prettyPrintLCDefs :: [LCDef] -> Text
prettyPrintLCDefs = T.intercalate "\n\n" . map prettyPrintLCDef

prettyPrintLCDef :: LCDef -> Text
prettyPrintLCDef (LCDef name expr) = show name <> " = " <> prettyPrintLCExpr expr

prettyPrintLCExpr :: LCExpr -> Text
prettyPrintLCExpr = \case
    Var name            -> show name
    --Fix vname expr      -> "fix (λ" <> show vname <> ". " <> prettyPrintLCExpr expr <> ")"
    Lambda vname expr   -> "λ" <> show vname <> ". " <> prettyPrintLCExpr expr
    Let vname expr body -> "let " <> show vname <> " = " <> prettyPrintLCExpr expr <> " in " <> prettyPrintLCExpr body
    App fexpr aexpr     -> prettyPrintLCExprParens fexpr <> " " <> prettyPrintLCExprParens aexpr
    IntLit i            -> show i
    Tuple as            -> "[" <> T.intercalate ", " (map prettyPrintLCExpr $ toList as) <> "]"
    Select i t          -> prettyPrintLCExprParens t <> "._" <> show i
    PrimOp p ps         -> "__" <> show p <> "__[" <> T.intercalate ", " (map prettyPrintLCExpr ps) <> "]"

prettyPrintLCExprParens :: LCExpr -> Text
prettyPrintLCExprParens = \case
    Var name -> prettyPrintLCExpr (Var name)
    Tuple as -> prettyPrintLCExpr (Tuple as)
    IntLit i -> prettyPrintLCExpr (IntLit i)
    x -> "(" <> prettyPrintLCExpr x <> ")"



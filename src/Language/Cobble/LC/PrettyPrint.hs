module Language.Cobble.LC.PrettyPrint where

import Language.Cobble.Prelude
import Language.Cobble.LC.Types

prettyPrintLCDefs :: [LCDef] -> Text
prettyPrintLCDefs defs = 

prettyPrintLCDef :: LCDef -> Text
prettyPrintLCDef (LCDef name expr) = name <> " = " <> prettyPrintLCExpr expr


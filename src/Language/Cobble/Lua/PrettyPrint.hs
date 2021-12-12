module Language.Cobble.Lua.PrettyPrint where

import Language.Cobble.Prelude
import Language.Cobble.Lua.Types

import Data.Text qualified as T


prettyLua :: [LuaStmnt] -> Text
prettyLua = unlines . map prettyLuaStmnt

-- Top level @Assign@s cannot be written as "local ..."
-- in the lua repl
prettyLuaForRepl :: [LuaStmnt] -> Text
prettyLuaForRepl = unlines . map \case
    Assign x e -> x <> " = " <> prettyLuaExpr e
    st -> prettyLuaStmnt st

prettyLuaStmnt :: LuaStmnt -> Text
prettyLuaStmnt = \case 
    Assign x e -> "local " <> x <> " = " <> prettyLuaExpr e
    DefFunction f xs body -> "function " <> f <> "(" <> T.intercalate ", " xs <> ")\n" 
        <> prettyLua body
        <> "end"
    Return e -> "return " <> prettyLuaExpr e 
    If c th el -> "if " <> prettyLuaExpr c <> " then\n"
        <> prettyLua th
        <> "else\n"
        <> prettyLua el 
        <> "end"

prettyLuaExpr :: LuaExpr -> Text
prettyLuaExpr = \case
    Function xs sts -> "(function (" <> T.intercalate ", " xs <> ")\n"
        <> prettyLua sts
        <> "end)"
    Call f es -> prettyLuaExpr f <> "(" <> T.intercalate ", " (map prettyLuaExpr es) <> ")"
    Var x -> x
    Table es -> "{" <> T.intercalate ", " (map prettyLuaExpr es) <> "}"
    TableIndex e n -> "(" <> prettyLuaExpr e <> ")[" <> show n <> "]"
    IntLit n -> show n
    BoolLit True -> "true"
    BoolLit False -> "false"
    Plus e1 e2 -> prettyLuaExpr e1 <> " + " <> prettyLuaExpr e2
    Minus e1 e2 -> prettyLuaExpr e1 <> " - " <> prettyLuaExpr e2
    Mul e1 e2 -> prettyLuaExpr e1 <> " * " <> prettyLuaExpr e2
    DivInt e1 e2 -> prettyLuaExpr e1 <> " // " <> prettyLuaExpr e2
    Mod e1 e2 -> prettyLuaExpr e1 <> " % " <> prettyLuaExpr e2
    LE e1 e2 -> prettyLuaExpr e1 <> " <= " <> prettyLuaExpr e2



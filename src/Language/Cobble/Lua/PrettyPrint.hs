module Language.Cobble.Lua.PrettyPrint where

import Language.Cobble.Prelude
import Language.Cobble.Lua.Types

import Data.Text qualified as T

header :: Text
header = unlines [
        "#!/usr/bin/env lua"
    ,   "function __setTestScoreboardUnsafe__(x)"
    ,   "    print(x)"
    ,   "end"
    ]

prettyLua :: [LuaStmnt] -> Text
prettyLua = (header <>) . prettyLuaStmnts 

prettyLuaStmnts :: [LuaStmnt] -> Text
prettyLuaStmnts = unlines . map prettyLuaStmnt

prettyLuaStmnt :: LuaStmnt -> Text
prettyLuaStmnt = \case 
    Assign x e -> "local " <> x <> " = " <> prettyLuaExpr e
    DefFunction f xs body -> "function " <> f <> "(" <> T.intercalate ", " xs <> ")\n" 
        <> prettyLuaStmnts body
        <> "end"
    Return e -> "return " <> prettyLuaExpr e 
    If c th el -> "if " <> prettyLuaExpr c <> " then\n"
        <> prettyLuaStmnts th
        <> "else\n"
        <> prettyLuaStmnts el 
        <> "end"

prettyLuaExpr :: LuaExpr -> Text
prettyLuaExpr = \case
    Function xs sts -> "(function (" <> T.intercalate ", " xs <> ")\n"
        <> prettyLuaStmnts sts
        <> "end)"
    Call f es -> prettyLuaExpr f <> "(" <> T.intercalate ", " (map prettyLuaExpr es) <> ")"
    Var x -> x
    Table es -> "{" <> T.intercalate ", " (map prettyLuaExpr es) <> "}"
    TableIndex e n -> "(" <> prettyLuaExpr e <> ")[" <> show n <> "]"
    IntLit n -> show n
    StringLit str -> show str
    BoolLit True -> "true"
    BoolLit False -> "false"
    Plus e1 e2 -> prettyLuaExpr e1 <> " + " <> prettyLuaExpr e2
    Minus e1 e2 -> prettyLuaExpr e1 <> " - " <> prettyLuaExpr e2
    Mul e1 e2 -> prettyLuaExpr e1 <> " * " <> prettyLuaExpr e2
    DivInt e1 e2 -> prettyLuaExpr e1 <> " // " <> prettyLuaExpr e2
    Mod e1 e2 -> prettyLuaExpr e1 <> " % " <> prettyLuaExpr e2
    LE e1 e2 -> prettyLuaExpr e1 <> " <= " <> prettyLuaExpr e2



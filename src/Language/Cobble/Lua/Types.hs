module Language.Cobble.Lua.Types where

import Language.Cobble.Prelude

data LuaStmnt = Assign Text LuaExpr                 -- local x = e
            | DefFunction Text [Text] [LuaStmnt]    -- function f(x,y) stmnt1 stmnt2 end
            | Return LuaExpr                        -- return e
            | If LuaExpr [LuaStmnt] [LuaStmnt]
            deriving (Show, Eq, Generic, Data)

data LuaExpr = Function [Text] [LuaStmnt]   -- (function (x,y) stmnt1 stmnt2 end)
             | Call LuaExpr [LuaExpr]       -- e1(e2, e3)
             | Var Text                     -- x
             | Table [LuaExpr]              -- {e1, e2}
             | TableIndex LuaExpr Int       -- (e)[n]
             | IntLit Int                   -- n
             | BoolLit Bool                 -- b 
             | Plus LuaExpr LuaExpr         -- e1 + e2
             | Minus LuaExpr LuaExpr        -- e1 - e2
             | Mul LuaExpr LuaExpr          -- e1 * e2
             | DivInt LuaExpr LuaExpr       -- e1 // e2
             | Mod LuaExpr LuaExpr          -- e1 % e2
             | LE LuaExpr LuaExpr           -- e1 <= e2
             deriving (Show, Eq, Generic, Data)

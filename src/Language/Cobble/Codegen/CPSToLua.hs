module Language.Cobble.Codegen.CPSToLua where

import Language.Cobble.Prelude
import Language.Cobble.Lua.Types as L
import Language.Cobble.CPS.Basic.Types as C
import Language.Cobble.Codegen.PrimOps as P

compile :: CPS -> [LuaStmnt]
compile (Let x e rest) = Assign (show x) (compileExpr e) : compile rest
compile (LetRec f k x body rest) = DefFunction (show f) [show k, show x] (compile body) : compile rest
compile (App3 f k x) = [Return (L.Call (compileVal f) [compileVal k, compileVal x])]
compile (App2 f x) = [Return (L.Call (compileVal f) [compileVal x])]
compile (C.If c th el) = [L.If (compileVal c) (compile th) (compile el)]

compileExpr :: CPSExpr -> LuaExpr
compileExpr (Val v) = compileVal v
compileExpr (Tuple vs) = Table (map compileVal vs)
compileExpr (Select i v) = TableIndex (compileVal v) (i + 1) -- lua arrays start at 1
compileExpr (PrimOp p vs) = compilePrimOp p vs

compileVal :: CPSVal -> LuaExpr
compileVal (C.IntLit i) = L.IntLit i
compileVal (C.Var qname) = L.Var (show qname)
compileVal (C.Lambda x y c) = L.Function [show x, show y] (compile c)
compileVal (C.Admin x c) = L.Function [show x] (compile c)
compileVal Halt = L.Function ["x"] [Return (L.Var "x")]

compilePrimOp :: PrimOp -> [CPSVal] -> LuaExpr
compilePrimOp True_ [] = BoolLit True
compilePrimOp False_ [] = BoolLit False
compilePrimOp Add [x, y] = Plus (compileVal x) (compileVal y)
compilePrimOp Sub [x, y] = Minus (compileVal x) (compileVal y)
compilePrimOp P.Mul [x, y] = L.Mul (compileVal x) (compileVal y)
compilePrimOp P.Div [x, y] = L.DivInt (compileVal x) (compileVal y) -- has to round down to keep integer semantics
compilePrimOp P.Mod [x, y] = L.Mod (compileVal x) (compileVal y)
compilePrimOp P.LE [x, y] = L.LE (compileVal x) (compileVal y)
compilePrimOp SetTestScoreboardUnsafe [x] = L.Call (L.Var "__setTestScoreboardUnsafe__") [compileVal x]
compilePrimOp p xs = error $ "CPSToLua.compilePrimOp: invalid combination of primop and arguments: " <> show (PrimOp p xs) 

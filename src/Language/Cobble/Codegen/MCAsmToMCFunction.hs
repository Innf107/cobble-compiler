module Language.Cobble.Codegen.MCAsmToMCFunction where

import Language.Cobble.Prelude hiding (to, from)
import Language.Cobble.Shared
import Language.Cobble.MCAsm.Types as A
import Language.Cobble.McFunction.Types as F

compile :: [Block] -> [(FilePath, [Command])]
compile blocks = map (\(Block n is) -> (qNameToPath n, concat $ run (traverse compileInstruction is))) blocks 

regs :: Objective
regs = "REGS"

compileInstruction :: Instruction -> Sem r [Command]
compileInstruction = \case
    Move to from                  -> pure [Scoreboard (Players (Operation (reg to) regs SAssign (reg from) regs))]
    MoveLit to n                  -> pure [Scoreboard (Players (Set (reg to) regs n))] 
    A.Add x y                     -> pure [Scoreboard (Players (Operation (reg x) regs SAdd (reg y) regs))] 
    AddLit x n                    -> pure [Scoreboard (Players (F.Add (reg x) regs n))] 
    Sub x y                       -> pure [Scoreboard (Players (Operation (reg x) regs SSub (reg y) regs))]
    SubLit x n                    -> pure [Scoreboard (Players (Remove (reg x) regs n))] 
    Mul x y                       -> pure [Scoreboard (Players (Operation (reg x) regs SMul (reg y) regs))] 
    Div x y                       -> pure [Scoreboard (Players (Operation (reg x) regs SDiv (reg y) regs))]  
    Mod x y                       -> pure [Scoreboard (Players (Operation (reg x) regs SMod (reg y) regs))]  
    Min x y                       -> pure [Scoreboard (Players (Operation (reg x) regs SMin (reg y) regs))]  
    Max x y                       -> pure [Scoreboard (Players (Operation (reg x) regs SMin (reg y) regs))]  
    Call f                        -> pure [Function f]  
    ICall x                       -> undefined  
    LoadFunctionAddress reg qn    -> undefined 
    CallInRange x r f             -> pure [Execute (EIf (IScore (reg x) regs (IMatches r)) (ERun (Function f)))] 
    CallEQ x y f                  -> pure [Execute (EIf (IScore (reg x) regs (IEQ (reg y) regs)) (ERun (Function f)))]  
    CallLT x y f                  -> pure [Execute (EIf (IScore (reg x) regs (ILT (reg y) regs)) (ERun (Function f)))]  
    CallGT x y f                  -> pure [Execute (EIf (IScore (reg x) regs (IGT (reg y) regs)) (ERun (Function f)))]  
    CallLE x y f                  -> pure [Execute (EIf (IScore (reg x) regs (ILE (reg y) regs)) (ERun (Function f)))]  
    CallGE x y f                  -> pure [Execute (EIf (IScore (reg x) regs (IGE (reg y) regs)) (ERun (Function f)))]  
    Malloc reg n                  -> undefined  
    Select x a i                  -> pure [Scoreboard (Players (Operation (reg x)       regs SAssign (arrayIX a i) regs))] 
    Store a x i                   -> pure [Scoreboard (Players (Operation (arrayIX a i) regs SAssign (reg x)       regs))] 


reg :: Register -> Selector
reg (Reg r)        = Player ("$" <> show r) 
reg (SpecialReg r) = Player ("%" <> r)

arrayIX :: Register -> Int -> Selector
arrayIX a i = Entity [] 

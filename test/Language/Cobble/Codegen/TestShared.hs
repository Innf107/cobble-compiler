module Language.Cobble.Codegen.TestShared where

import Language.Cobble.Prelude
import Language.Cobble.LC.Types as L
import Language.Cobble.CPS.Basic.Types as C
import Language.Cobble.CPS.TopLevel.Types as T
import Language.Cobble.MCAsm.Types as A 
import Language.Cobble.McFunction.Types

exampleLC :: LCExpr
exampleLC = L.App (L.Lambda "a" (L.Select 1 (L.Var "a"))) (L.Tuple [L.IntLit 3, L.IntLit 4])
-- (\a -> a#1) (3, 4) 

exampleCPS :: CPS
exampleCPS = C.App2
                (C.Admin "f0" (C.App2
                    (C.Admin "x0" (C.App2
                        (C.Admin "x1" (C.Let "t2" (C.Tuple [C.Var "x0", C.Var "x1"]) (C.App2
                            (C.Admin "v1" (C.App3 (C.Var "f0") C.Halt (C.Var "v1")))
                            (C.Var "t2")
                            )))
                        (C.IntLit 4)))
                    (C.IntLit 3)))
                (C.Lambda "k3" "a" (C.App2 (Admin "t4" (C.Let "y5" (C.Select 1 (C.Var "t4")) (C.App2 (C.Var "k3") (C.Var "y5")))) (C.Var "a")))

exampleReduced :: CPS
exampleReduced = C.Let "f0" (Val (C.Lambda "k3" "a" (C.Let "y5" (C.Select 1 (C.Var "a")) (C.App2 (C.Var "k3") (C.Var "y5")))))
                    (C.Let "x0" (Val (C.IntLit 3))
                        (C.Let "x1" (Val (C.IntLit 4))
                            (C.Let "t2" (C.Tuple [C.Var "x0", C.Var "x1"])
                                (C.App3 (C.Var "f0") C.Halt (C.Var "t2")))))
exampleReducedFreshIX :: Int
exampleReducedFreshIX = 6

exampleTL :: TL
exampleTL = T.LetF "f6" "k3" ["s7", "a"] (T.Let "y5" (T.Select 1 "a")
                (T.Let "k39" (T.Select 0 "k3")
                (T.Let "env10" (T.Select 1 "k3")
                (T.App "k39" ["env10", "y5"]))))
            (T.C 
                (T.Let "env8" (T.Tuple [])
                (T.Let "f0" (T.Tuple ["f6", "env8"])
                (T.Let "x0" (T.IntLit 3)
                (T.Let "x1" (T.IntLit 4)
                (T.Let "t2" (T.Tuple ["x0", "x1"])
                (T.Let "h11" T.Halt
                (T.Let "henv12" (T.Tuple [])
                (T.Let "e13" (T.Tuple ["h11", "henv12"])
                (T.Let "f014" (T.Select 0 "f0")
                (T.Let "env15" (T.Select 1 "f0")
                (T.App "f014" ["e13", "env15", "t2"])
                )))))))))))

exampleASM :: [Block]
exampleASM = [
        Block "f6" [
            Move (Reg "k3") (SpecialReg "arg0")
        ,   Move (Reg "s7") (SpecialReg "arg1")
        ,   Move (Reg "a")  (SpecialReg "arg2")

        ,   A.Select (Reg "y5") (Reg "a") 1
        ,   A.Select (Reg "k39") (Reg "k3") 0
        ,   A.Select (Reg "env10") (Reg "k3") 1

        ,   Move (SpecialReg "arg0") (Reg "env10")
        ,   Move (SpecialReg "arg1") (Reg "y5")
        ,   ICall (Reg "k39")
        ]
    ,   Block "__main__" [
            LoadFunctionAddress (Reg "f6") "f6"

        ,   Malloc (Reg "env8") 0
        
        ,   Malloc (Reg "f0") 2
        ,   Store (Reg "f0") (Reg "f6") 0
        ,   Store (Reg "f0") (Reg "env8") 1

        ,   MoveLit (Reg "x0") 3
        
        ,   MoveLit (Reg "x1") 4
        
        ,   Malloc (Reg "t2") 2
        ,   Store (Reg "t2") (Reg "x0") 0
        ,   Store (Reg "t2") (Reg "x1") 1

        ,   MoveLit (Reg "h11") 0
        ,   Malloc (Reg "henv12") 0
        
        ,   Malloc (Reg "e13") 2
        ,   Store (Reg "e13") (Reg "h11") 0
        ,   Store (Reg "e13") (Reg "henv12") 1

        ,   A.Select (Reg "f014") (Reg "f0") 0
        ,   A.Select (Reg "env15") (Reg "f0") 1

        ,   Move (SpecialReg "arg0") (Reg "e13")
        ,   Move (SpecialReg "arg1") (Reg "env15")
        ,   Move (SpecialReg "arg2") (Reg "t2")
        ,   ICall (Reg "f014")
        ]
    ]

exampleMCFunction :: [CompiledModule]
exampleMCFunction = [
        ("f6",[
            Scoreboard $ Players $ Operation (Player "$k3") "REGS" SAssign (Player "%arg0") "REGS"
        ,   Scoreboard $ Players $ Operation (Player "$s7") "REGS" SAssign (Player "%arg1") "REGS"
        ,   Scoreboard $ Players $ Operation (Player "$a")  "REGS" SAssign (Player "%arg2") "REGS"

        ,   Execute $ EAs (Entity [SScores [("IX", 0)]]) $ EIf (IScore (Self []) "REGS" $ IEQ (Player "$a") "REGS") 
                    $ ERun (Scoreboard $ Players $ Operation (Player "$y5") "REGS" SAssign (Self []) "REGS")

        ,   Scoreboard $ Players $ Operation (Player "%arg0")  "REGS" SAssign (Player "$y5") "REGS"
        -- TODO: ICall is NYI
        ])
    ]


module Language.Cobble.Codegen.TestShared where

import Language.Cobble.Prelude
import Language.Cobble.LC.Types as L
import Language.Cobble.CPS.Basic.Types as C
import Language.Cobble.CPS.TopLevel.Types as T
import Language.Cobble.MCAsm.Types as A 
import Language.Cobble.McFunction.Types
import Language.Cobble.TestUtil
import Language.Cobble.Types.QualifiedName

exampleLC :: LCExpr
exampleLC = L.App (L.Lambda "a" (L.Select 1 (L.Var "a"))) (L.Tuple [L.IntLit 3, L.IntLit 4])
-- (\a -> a#1) (3, 4) 

exampleCPS :: CPS
exampleCPS = C.App2
                (C.Admin "f" (C.App2
                    (C.Admin "x0" (C.App2
                        (C.Admin "x1" (C.Let "t" (C.Tuple [C.Var "x0", C.Var "x1"]) (C.App2
                            (C.Admin "v" (C.App3 (C.Var "f") C.Halt (C.Var "v")))
                            (C.Var "t")
                            )))
                        (C.IntLit 4)))
                    (C.IntLit 3)))
                (C.Lambda "k" "a" (C.App2 (Admin "t" (C.Let "y" (C.Select 1 (C.Var "t")) (C.App2 (C.Var "k") (C.Var "y")))) (C.Var "a")))

exampleReduced :: CPS
exampleReduced = C.Let "f" (Val (C.Lambda "k" "a" (C.Let "y" (C.Select 1 (C.Var "a")) (C.App2 (C.Var "k") (C.Var "y")))))
                    (C.Let "x0" (Val (C.IntLit 3))
                        (C.Let "x1" (Val (C.IntLit 4))
                            (C.Let "t" (C.Tuple [C.Var "x0", C.Var "x1"])
                                (C.App3 (C.Var "f") C.Halt (C.Var "t")))))

(~=) :: QualifiedName -> QualifiedName -> Bool
n ~= m = originalName n == originalName m

cpsEqual :: CPS -> CPS -> Bool
cpsEqual = curry \case 
    (C.Let f e b, C.Let f' e' b') -> f ~= f' && cpsEqualE e e' && cpsEqual b b'
    (C.LetRec f k x b e, C.LetRec f' k' x' b' e') -> f ~= f' && x ~= x' && k ~= k' && b `cpsEqual` b' && e `cpsEqual` e'
    (App3 f k x, App3 f' k' x') -> f `cpsEqualV` f' && k `cpsEqualV` k' && x `cpsEqualV` x'
    (App2 c x, App2 c' x') -> c `cpsEqualV` c' && x `cpsEqualV` x'
    (C.If c th el, C.If c' th' el') -> c `cpsEqualV` c' && th `cpsEqual` th' && el `cpsEqual` el'
    _ -> False 

cpsEqualE :: CPSExpr -> CPSExpr -> Bool 
cpsEqualE = curry \case
    (Val v, Val v') -> v `cpsEqualV` v'
    (C.Tuple vs, C.Tuple vs') -> all id (zipWith cpsEqualV vs vs')
    (C.Select x v, C.Select x' v') -> x == x' && v `cpsEqualV` v'
    (C.PrimOp p vs, C.PrimOp p' vs') -> p == p' && all id (zipWith cpsEqualV vs vs')
    _ -> False

cpsEqualV :: CPSVal -> CPSVal -> Bool
cpsEqualV = curry \case
    (C.IntLit n, C.IntLit n') -> n == n'
    (C.Var v, C.Var v') -> v ~= v'
    (C.Lambda k x b, C.Lambda k' x' b') -> k ~= k' && x ~= x' && b `cpsEqual` b'
    (Admin x b, Admin x' b') -> x ~= x' && b `cpsEqual` b'
    (C.Halt, C.Halt) -> True
    _ -> False


exampleReducedFreshIX :: Int
exampleReducedFreshIX = 6

exampleTL :: TL
exampleTL = T.LetF "f_6" "k" ["s_7", "a"] (T.Let "y" (T.Select 1 "a")
                (T.Let "k_9" (T.Select 0 "k")
                (T.Let "env_10" (T.Select 1 "k")
                (T.App "k_9" ["env_10", "y"]))))
            (T.C 
                (T.Let "env_8" (T.Tuple [])
                (T.Let "f" (T.Tuple ["f_6", "env_8"])
                (T.Let "x0" (T.IntLit 3)
                (T.Let "x1" (T.IntLit 4)
                (T.Let "t" (T.Tuple ["x0", "x1"])
                (T.Let "h_11" T.Halt
                (T.Let "henv_12" (T.Tuple [])
                (T.Let "e_13" (T.Tuple ["h_11", "henv_12"])
                (T.Let "f_14" (T.Select 0 "f")
                (T.Let "env_15" (T.Select 1 "f")
                (T.App "f_14" ["e_13", "env_15", "t"])
                )))))))))))

exampleASM :: [Block]
exampleASM = [
        Block "f_6" [
            Move (Reg "k_5") (SpecialReg "arg0")
        ,   Move (Reg "s_7") (SpecialReg "arg1")
        ,   Move (Reg "a")  (SpecialReg "arg2")

        ,   A.Select (Reg "y_7") (Reg "a") 1
        ,   A.Select (Reg "k_5_9") (Reg "k_5") 0
        ,   A.Select (Reg "env_10") (Reg "k_5") 1

        ,   Move (SpecialReg "arg0") (Reg "env_10")
        ,   Move (SpecialReg "arg1") (Reg "y_7")
        ,   ICall (Reg "k_5_9")
        ]
    ,   Block "__main__" [
            LoadFunctionAddress (Reg "f_6") "f_6"

        ,   Malloc (Reg "env_8") 0
        
        ,   Malloc (Reg "f_0") 2
        ,   Store (Reg "f_0") (Reg "f_6") 0
        ,   Store (Reg "f_0") (Reg "env_8") 1

        ,   MoveLit (Reg "x0_3") 3
        
        ,   MoveLit (Reg "x1_4") 4
        
        ,   Malloc (Reg "t_2") 2
        ,   Store (Reg "t_2") (Reg "x0_3") 0
        ,   Store (Reg "t_2") (Reg "x1_4") 1

        ,   MoveLit (Reg "h_11") 0
        ,   Malloc (Reg "henv_12") 0
        
        ,   Malloc (Reg "e_13") 2
        ,   Store (Reg "e_13") (Reg "h_11") 0
        ,   Store (Reg "e_13") (Reg "henv_12") 1

        ,   A.Select (Reg "f_0_14") (Reg "f_0") 0
        ,   A.Select (Reg "env_15") (Reg "f_0") 1

        ,   Move (SpecialReg "arg0") (Reg "e_13")
        ,   Move (SpecialReg "arg1") (Reg "env_15")
        ,   Move (SpecialReg "arg2") (Reg "t_2")
        ,   ICall (Reg "f_0_14")
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


module Language.Cobble.Codegen.TestShared where

import Language.Cobble.Prelude
import Language.Cobble.LC.Types as L
import Language.Cobble.CPS.Basic.Types as C
import Language.Cobble.CPS.TopLevel.Types as T

exampleLC :: LCExpr
exampleLC = L.App (L.Lambda "a" (L.Select 1 (L.Var "a"))) (L.Tuple [L.IntLit 3, L.IntLit 4])


exampleCPS :: CPS
exampleCPS = C.App2
                (C.Admin "f0" (C.App2
                    (C.Admin "x0" (C.App2
                        (C.Admin "x1" (C.Let "t2" (C.Tuple [C.Var "x0", C.Var "x1"]) (C.App2
                            (C.Admin "v1" (C.App3 (C.Var "f0") (C.Var "v1") C.Halt))
                            (C.Var "t2")
                            )))
                        (C.IntLit 4)))
                    (C.IntLit 3)))
                (C.Lambda "a" "k3" (C.App2 (Admin "t4" (C.Let "y5" (C.Select 1 (C.Var "t4")) (C.App2 (C.Var "k3") (C.Var "y5")))) (C.Var "a")))

exampleReduced :: CPS
exampleReduced = C.Let "f0" (Val (C.Lambda "a" "k3" (C.Let "y5" (C.Select 1 (C.Var "a")) (C.App2 (C.Var "k3") (C.Var "y5")))))
                    (C.Let "x0" (Val (C.IntLit 3))
                        (C.Let "x1" (Val (C.IntLit 4))
                            (C.Let "t2" (C.Tuple [C.Var "x0", C.Var "x1"])
                                (C.App3 (C.Var "f0") (C.Var "t2") C.Halt))))

exampleTL :: TL
exampleTL = T.LetF "f0" ["s1", "a", "k3"] (T.Let "y5" (T.Select 1 "a") 
                (T.App "k3" ["y5"]))     
            (T.C 
                (T.Let "x0" (T.IntLit 3)
                (T.Let "x1" (T.IntLit 4)
                (T.Let "t2" (T.Tuple ["x0", "x1"])
                (T.Let "env" (T.Tuple [])
                (T.Let "v2" T.Halt
                (T.App "f0" ["env", "t2", "v2"])
                ))))))

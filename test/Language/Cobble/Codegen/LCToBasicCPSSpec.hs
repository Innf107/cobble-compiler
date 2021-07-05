module Language.Cobble.Codegen.LCToBasicCPSSpec where

import Language.Cobble.Prelude
import Language.Cobble.LC.Types as L
import Language.Cobble.CPS.Basic.Types as C
import Language.Cobble.Codegen.LCToBasicCPS

import Test.Hspec

spec :: Spec
spec = do
    let exampleLC = L.App (L.Lambda "a" (L.Select 1 (L.Var "a"))) (L.Tuple [L.IntLit 3, L.IntLit 4])
    let exampleCPS = C.App2
                        (C.Admin "f" (C.App2
                            (C.Admin "x0" (C.App2
                                (C.Admin "x1" (C.Let "t" (C.Tuple [C.Var "x0", C.Var "x1"]) (C.App2
                                    (C.Admin "v" (C.App3 (C.Var "f") (C.Var "v") Halt))
                                    (C.Var "t")
                                    )))
                                (C.IntLit 4)))
                            (C.IntLit 3)))
                        (C.Lambda "a" "k'" (C.App2 (Admin "t" (Let "y" (C.Select 1 (C.Var "t")) (C.App2 (C.Var "k'") (C.Var "y")))) (C.Var "a")))
    let exampleReduced = C.Let "f" (Val (C.Lambda "a" "k'" (Let "y" (C.Select 1 (C.Var "a")) (C.App2 (C.Var "k'") (C.Var "y")))))
                            (C.Let "x0" (Val (C.IntLit 3))
                                (C.Let "x1" (Val (C.IntLit 4))
                                    (C.Let "t" (C.Tuple [C.Var "x0", C.Var "x1"])
                                        (C.App3 (C.Var "f") (C.Var "t") Halt))))
    describe "compileExpr" do
        it "correctly compiles '(λa.(#1 a)) (3,4)'" do
            compileExpr exampleLC Halt `shouldBe` exampleCPS
    describe "reduceAdmin" do
        it "reduces all unnecessary admins in '(λ_f.(λ_x1.(λ_x2.let b = (x1,x2) in (λ_v.(f v k)) b) 4) 3) (λak′.[[a]](λ_t.let y = #1 t in k′ y))'" do
            reduceAdmin exampleCPS `shouldBe` exampleReduced

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
                        (C.Admin "f0" (C.App2
                            (C.Admin "x0" (C.App2
                                (C.Admin "x1" (C.Let "t2" (C.Tuple [C.Var "x0", C.Var "x1"]) (C.App2
                                    (C.Admin "v1" (C.App3 (C.Var "f0") (C.Var "v1") Halt))
                                    (C.Var "t2")
                                    )))
                                (C.IntLit 4)))
                            (C.IntLit 3)))
                        (C.Lambda "a" "k3" (C.App2 (Admin "t4" (Let "y5" (C.Select 1 (C.Var "t4")) (C.App2 (C.Var "k3") (C.Var "y5")))) (C.Var "a")))
    let exampleReduced = C.Let "f0" (Val (C.Lambda "a" "k3" (Let "y5" (C.Select 1 (C.Var "a")) (C.App2 (C.Var "k3") (C.Var "y5")))))
                            (C.Let "x0" (Val (C.IntLit 3))
                                (C.Let "x1" (Val (C.IntLit 4))
                                    (C.Let "t2" (C.Tuple [C.Var "x0", C.Var "x1"])
                                        (C.App3 (C.Var "f0") (C.Var "t2") Halt))))
    describe "compileExpr" do
        it "correctly compiles '(λa.(#1 a)) (3,4)'" do
            runTest (compileExpr exampleLC Halt) `shouldBe` exampleCPS
    describe "reduceAdmin" do
        it "reduces all unnecessary admins in '(λ_f.(λ_x1.(λ_x2.let b = (x1,x2) in (λ_v.(f v k)) b) 4) 3) (λak′.[[a]](λ_t.let y = #1 t in k′ y))'" do
            reduceAdmin exampleCPS `shouldBe` exampleReduced
    describe "freshVar" do
        it "appends the current state to the last name component" do
            run (evalState @Int 3 (freshVar "test.x")) `shouldBe` "test.x3"
        it "increments the state" do
            run (execState @Int 3 (freshVar "test.x.y")) `shouldBe` 4


runTest :: Sem '[State Int] a -> a
runTest = run . evalState 0

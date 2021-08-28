module Language.Cobble.Codegen.LCToBasicCPSSpec where

import Language.Cobble.Prelude
import Language.Cobble.Types.QualifiedName
import Language.Cobble.Types.LexInfo
import Language.Cobble.LC.Types as L
import Language.Cobble.CPS.Basic.Types as C
import Language.Cobble.Codegen.LCToBasicCPS
import Language.Cobble.Codegen.TestShared
import Language.Cobble.Util.Polysemy.Fresh

import Test.Hspec

spec :: Spec
spec = do
    describe "compileExpr" do
        it "correctly compiles '(λa.(#1 a)) (3,4)'" do
            runTest (compileExpr exampleLC Halt) `shouldSatisfy` (cpsEqual exampleCPS)
    describe "reduceAdmin" do
        it "reduces all unnecessary admins in '(λ_f.(λ_x1.(λ_x2.let b = (x1,x2) in (λ_v.(f v k)) b) 4) 3) (λak′.[[a]](λ_t.let y = #1 t in k′ y))'" do
            reduceAdmin exampleCPS `shouldSatisfy` (cpsEqual exampleReduced)



runTest :: Sem '[Fresh Text QualifiedName, Fresh (Text, LexInfo) QualifiedName] a -> a
runTest = run . runFreshQNamesState . freshWithInternal

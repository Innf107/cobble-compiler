module Language.Cobble.Codegen.BasicCPSToTopLevelCPSSpec where

import Language.Cobble.Codegen.BasicCPSToTopLevelCPS
import Language.Cobble.Codegen.TestShared

import Language.Cobble.Prelude
import Language.Cobble.CPS.Basic.Types
import Language.Cobble.CPS.TopLevel.Types
import Test.Hspec

spec :: Spec
spec = do
    describe "compile" do
        it ("correctly compiles " <> show exampleReduced) do
            run (evalState (0::Int) (compile exampleReduced))
                `shouldBe`
                exampleTL
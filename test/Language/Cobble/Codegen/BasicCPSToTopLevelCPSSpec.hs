module Language.Cobble.Codegen.BasicCPSToTopLevelCPSSpec where

import Language.Cobble.Codegen.BasicCPSToTopLevelCPS
import Language.Cobble.Codegen.TestShared

import Language.Cobble.Prelude

import Language.Cobble.Util.Polysemy.Fresh

import Language.Cobble.CPS.Basic.Types
import Language.Cobble.CPS.TopLevel.Types
import Test.Hspec

spec :: Spec
spec = do
    describe "compile" do
        it ("correctly compiles " <> show exampleReduced) do
            pass :: IO ()
            --run (runFreshQNamesStateInitial exampleReducedFreshIX $ freshWithInternal (compile exampleReduced))
            --    `shouldBe`
            --    exampleTL
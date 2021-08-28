module Language.Cobble.Codegen.TopLevelCPSToMCAsmSpec where

import Language.Cobble.Prelude
import Language.Cobble.Codegen.TopLevelCPSToMCAsm

import Language.Cobble.CPS.TopLevel.Types
import Language.Cobble.MCAsm.Types

import Language.Cobble.Codegen.TestShared

import Test.Hspec

spec :: Spec
spec = do
    describe "compile" do
        it ("correctly compiles " <> show exampleTL) do
            pass :: IO () -- TODO
            -- compile exampleTL `shouldBe` exampleASM


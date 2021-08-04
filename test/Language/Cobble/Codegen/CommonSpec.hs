module Language.Cobble.Codegen.CommonSpec where

import Language.Cobble.Codegen.Common

import Language.Cobble.Prelude

import Test.Hspec

spec :: Spec 
spec = do
    describe "freshVar" do
        it "appends the current state to the last name component" do
            run (evalState @Int 3 (freshVar "test.x")) `shouldBe` "test.x3"
        it "increments the state" do
            run (execState @Int 3 (freshVar "test.x.y")) `shouldBe` 4

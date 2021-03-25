module Language.Cobble.SharedSpec where

import Language.Cobble.Prelude

import Language.Cobble.Shared

import Test.Hspec

spec :: Spec
spec = do
    describe "show" do
        it "combines the names with '.'" do
            show (QualifiedName ["Test", "A", "B", "C"]) `shouldBe` "Test.A.B.C"
    describe "prependQual" do
        it "prepends to the Name (sets it as the first component)" do
            prependQual "New" (QualifiedName ["Test", "A", "B", "C"]) `shouldBe` QualifiedName ["New", "Test", "A", "B", "C"]
    describe "(<>)" do
        it "concatenates the components of both names" do
            QualifiedName ["A", "B"] <> QualifiedName ["C", "D", "E"] `shouldBe` QualifiedName ["A", "B", "C", "D", "E"]
        it "is associative" do
            (QualifiedName ["A", "B"] <> QualifiedName ["C", "D"]) <> QualifiedName ["E"]
                `shouldBe` QualifiedName ["A", "B"] <> (QualifiedName ["C", "D"] <> QualifiedName ["E"])
    describe "mempty" do
        it "satisfies the monoid identity laws" do
            QualifiedName ["A", "B"] <> mempty `shouldBe` QualifiedName ["A", "B"]
            mempty <> QualifiedName ["A", "B"] `shouldBe` QualifiedName ["A", "B"]
    describe "(.:)" do
        it "appends the Text to the right of the name" do
            QualifiedName ["Test", "A", "B", "C"] .: "D" `shouldBe` QualifiedName ["Test", "A", "B", "C", "D"]

    describe "makeQName" do
        it "splits the name at ." do
            makeQName "Test.A.B.C.D" `shouldBe` QualifiedName ["Test", "A", "B", "C", "D"]
        it "returns mempty for an empty string" do
            makeQName "" `shouldBe` mempty

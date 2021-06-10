{-#LANGUAGE NoImplicitPrelude, BlockArguments, OverloadedStrings#-}
{-# LANGUAGE TypeApplications, DataKinds #-}
module Language.Cobble.Types.PrettyPrintSpec where

import Language.Cobble.Prelude
import Language.Cobble.Types
import Language.Cobble.Parser.Tokenizer

import Language.Cobble.Types.PrettyPrint

import Test.Hspec

spec :: Spec
spec = do
    describe "prettyPrintToken" do
        it "prints `Ident` without modification" do
            prettyPrintToken (Token dli (Ident "x")) `shouldBe` "x"
        it "prints `Reserved` without modification" do
            prettyPrintToken (Token dli (Reserved "while")) `shouldBe` "while"
        it "prints `Paren` without modification" do
            prettyPrintToken (Token dli (Paren "(")) `shouldBe` "("
        it "prints `Operator` without modification" do
            prettyPrintToken (Token dli (Operator "+")) `shouldBe` "+"
        it "prints `ReservedOp` without modification" do
            prettyPrintToken (Token dli (ReservedOp ":")) `shouldBe` ":"
        it "prints `IntLiteral` without modification" do
            prettyPrintToken (Token dli (IntLiteral 5)) `shouldBe` "5"

dli :: LexInfo
dli = LexInfo (SourcePos 0 0) (SourcePos 0 0) "Test"

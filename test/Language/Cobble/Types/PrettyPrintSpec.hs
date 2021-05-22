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
    describe "prettyPrint for Tokens" do
        it "behaves exactly the same way as prettyPrintToken" do
            traverse_ (\x -> prettyPrintToken x `shouldBe` prettyPrint x) [
                  Token dli (Ident "x")
                , Token dli (Reserved "while")
                , Token dli (Paren "(")
                , Token dli (Operator "+")
                , Token dli (ReservedOp ":")
                , Token dli (IntLiteral 5)
                ]
    describe "prettyPrint for Name" do
        it "gives the right value for each compiler pass (currently tested: ParsePreprocess, ExpandMacros, QualifyNames, Typecheck, Codegen)" do
            prettyPrint ("Test" :: Name 'QualifyNames) `shouldBe` "Test"
            prettyPrint ("Some.Other.Test" :: Name 'QualifyNames) `shouldBe` "Some.Other.Test"
            prettyPrint ("Test" :: Name 'Typecheck) `shouldBe` "Test"
            prettyPrint ("Some.Other.Test" :: Name 'Typecheck) `shouldBe` "Some.Other.Test"
            prettyPrint ("Test" :: Name 'Codegen) `shouldBe` "Test"
            prettyPrint ("Some.Other.Test" :: Name 'Codegen) `shouldBe` "Some.Other.Test"

    describe "prettyPrint for Expressions" do
        describe "IntLit" do
            context "For all unchanged passes" do
                it "just converts the literal to a text" do
                    prettyPrint (IntLit () dli 42 :: Expr 'QualifyNames) `shouldBe` "42"
                    prettyPrint (IntLit () dli 42 :: Expr 'Typecheck) `shouldBe` "42"
                    prettyPrint (IntLit () dli 42 :: Expr 'Codegen) `shouldBe` "42"
        {-
        describe "BoolLit" do
            context "For all unchanged passes" do
                it "converts the literal to a text (lowercase)" do
                    prettyPrint (BoolLit () dli True :: Expr 'QualifyNames) `shouldBe` "true"
                    prettyPrint (BoolLit () dli False :: Expr 'QualifyNames) `shouldBe` "false"
                    prettyPrint (BoolLit () dli True :: Expr 'Typecheck) `shouldBe` "true"
                    prettyPrint (BoolLit () dli False :: Expr 'Typecheck) `shouldBe` "false"
                    prettyPrint (BoolLit () dli True :: Expr 'Codegen) `shouldBe` "true"
                    prettyPrint (BoolLit () dli False :: Expr 'Codegen) `shouldBe` "false"-}
        describe "Var" do
            context "For all unchanged passes" do
                it "just takes the variable name" do
                    prettyPrint (Var () dli "x" :: Expr 'QualifyNames) `shouldBe` "x"
                    prettyPrint (Var () dli "x" :: Expr 'Typecheck) `shouldBe` "x"
            context "For the Codegen pass" do
                it "adds the type information in a comment" do
                    prettyPrint (Var intT dli "x" :: Expr 'Codegen) `shouldBe` "x {-: prims.Int (:: *)-}"
                    prettyPrint (Var (TCon "T" KStar) dli "x" :: Expr 'Codegen) `shouldBe` "x {-: T (:: *)-}"
        {-
        describe "FCall" do
            context "For all unchanged passes" do
                it "displays the function call like a C-like language " do
                    prettyPrint (FCall () dli (Var () dli "f") [Var () dli "x", IntLit () dli 5] :: Expr 'QualifyNames) `shouldBe` "f(x, 5)"
                    prettyPrint (FCall () dli (Var () dli "f") [Var () dli "x", IntLit () dli 5] :: Expr 'Typecheck) `shouldBe` "f(x, 5)"
            context "For the Codegen pass" do
                it "adds the type information in a comment" do
                    prettyPrint (FCall intT dli (Var (unitT -:> intT) dli "f") [] :: Expr 'Codegen) `shouldBe` "f() {-: prims.Int (:: *)-}"
                it "adds type information for its arguments" do
                    prettyPrint (FCall intT dli (Var (intT -:> intT -:> intT) dli "f") [Var intT dli "x", IntLit () dli 5] :: Expr 'Codegen)
                        `shouldBe` "f(x {-: prims.Int (:: *)-}, 5) {-: prims.Int (:: *)-}"
    describe "PrettyPrint on Statements" do
        pass -- TODO
-}

dli :: LexInfo
dli = LexInfo (SourcePos 0 0) (SourcePos 0 0) "Test"

{-# LANGUAGE NoImplicitPrelude, DataKinds, BlockArguments, OverloadedStrings #-}

module Language.MCScript.TypecheckerSpec where

import Language.MCScript.Prelude
import Language.MCScript.Typechecker as TC
import Language.MCScript.Types

import Test.Hspec as S

spec :: Spec
spec = do
    describe "Decl" do
        it "does not fail for correct types" do
            runTypecheck [Decl "x" IntT (IntLit 5)] `shouldSatisfy` isRight
            runTypecheck [Decl "y" BoolT (BoolLit True)] `shouldSatisfy` isRight
        it "returns a typed, otherwise unchanged statement with correct types for the expression" do
            runTypecheck [Decl "x" IntT (IntLit 23)] `shouldBe` Right [Decl "x" IntT (IntLit 23, IntT)]
            runTypecheck [Decl "x" BoolT (BoolLit False)] `shouldBe` Right [Decl "x" BoolT (BoolLit False, BoolT)]
        it "fails for incorrect types" do
            runTypecheck [Decl "x" IntT (BoolLit True)] `shouldSatisfy` isLeft
            runTypecheck [Decl "x" BoolT (IntLit 2)] `shouldSatisfy` isLeft
        it "gives correct error messages" do
            runTypecheck [Decl "x" IntT (BoolLit True)] `shouldBe` Left (WrongDeclType "x" IntT BoolT)
            runTypecheck [Decl "y" BoolT (IntLit 45)] `shouldBe` Left (WrongDeclType "y" BoolT IntT)
        it "propagates errors" do
            runTypecheck [Decl "x" IntT (FCall "doesNotExist" [])] `shouldBe` Left (FunctionDoesNotExist "doesNotExist")
            runTypecheck [Decl "y" BoolT (FCall "doesNotExist" [])] `shouldBe` Left (FunctionDoesNotExist "doesNotExist")
    describe "Assign" do
        context "when a variable was previosly declared" do
            it "does not fail for correct types" do
                runTypecheck [Decl "x" IntT (IntLit 5), Assign "x" (IntLit 7)] `shouldSatisfy` isRight
                runTypecheck [Decl "x" BoolT (BoolLit True), Assign "x" (BoolLit False)] `shouldSatisfy` isRight
            it "returns a typed, otherwise unchanged statements with correct types for the expressions" do
                runTypecheck [Decl "x" IntT (IntLit 5), Assign "x" (IntLit 7)]
                    `shouldBe`
                    Right [Decl "x" IntT (IntLit 5, IntT), Assign "x" (IntLit 7, IntT)]
            it "fails for incorrect types" do
                runTypecheck [Decl "x" IntT (IntLit 5), Assign "x" (BoolLit False)]
                    `shouldSatisfy`
                    isLeft
                runTypecheck [Decl "x" BoolT (BoolLit True), Assign "x" (IntLit 2)]
                    `shouldSatisfy`
                    isLeft
            it "gives correct error messages" do
                runTypecheck [Decl "x" IntT (IntLit 5), Assign "x" (BoolLit False)]
                    `shouldBe`
                    Left (WrongAssignType "x" IntT BoolT)
                runTypecheck [Decl "x" BoolT (BoolLit True), Assign "x" (IntLit 2)]
                    `shouldBe`
                    Left (WrongAssignType "x" BoolT IntT)
            it "propagates errors in the assigned expression" do
                runTypecheck [Decl "x" IntT (IntLit 5), Assign "x" (FCall "doesNotExist" [])]
                    `shouldBe`
                    Left (FunctionDoesNotExist "doesNotExist")
                runTypecheck [Decl "x" BoolT (BoolLit False), Assign "x" (FCall "doesNotExist" [])]
                    `shouldBe`
                    Left (FunctionDoesNotExist "doesNotExist")
        context "with a free variable" do
            it "fails" do
                runTypecheck [Assign "x" (IntLit 3)] `shouldSatisfy` isLeft
            it "creates an apropriate error message" do
                runTypecheck [Assign "x" (IntLit 3)] `shouldBe` Left (VarDoesNotExist "x")
            it "fails before any erors in the expression" do
                runTypecheck [Assign "x" (FCall "doesNotExist" [])] `shouldBe` Left (VarDoesNotExist "x")

    describe "DefVoid" do
        context "without parameters" do
            it "does not fail if the body is correct" do
                runTypecheck [DefVoid "f" [] [Decl "x" IntT (IntLit 5)]] `shouldSatisfy` isRight
            it "gives back a correctly typed, otherwise unchanged statement" do
                runTypecheck [DefVoid "f" [] [Decl "x" IntT (IntLit 5), Assign "x" (IntLit 4)]]
                    `shouldBe`
                    Right [DefVoid "f" [] [Decl "x" IntT (IntLit 5, IntT), Assign "x" (IntLit 4, IntT)]]
            it "propagates errors in the body" do
                runTypecheck [DefVoid "f" [] [Decl "x" IntT (IntLit 5), Assign "x" (BoolLit False)]]
                    `shouldBe`
                    Left (WrongAssignType "x" IntT BoolT)
            it "is available inside the body (allows for recursion)" do
                runTypecheck [DefVoid "f" [] [CallFun "f" []]]
                    `shouldBe`
                    Right [DefVoid "f" [] [CallFun "f" []]]
        context "with parameters" do
            it "makes its parameters available in its body" do
                runTypecheck [DefVoid "f" [("x", IntT)] [Decl "y" IntT (Var "x")]]
                    `shouldBe`
                    Right [DefVoid "f" [("x", IntT)] [Decl "y" IntT (Var "x", IntT)]]
            it "has the correct type in its body (recursion)" do
                runTypecheck [DefVoid "f" [("x", IntT), ("y", BoolT)] [CallFun "f" [Var "x", Var "y"]]]
                    `shouldBe`
                    Right [DefVoid "f" [("x", IntT), ("y", BoolT)] [CallFun "f" [(Var "x", IntT), (Var "y", IntT)]]]
    describe "DefFun" do
        context "without parameters" do
            it "does not fail if the body and the return type are correct" do
                runTypecheck [DefFun "f" [] [Decl "x" IntT (IntLit 3), Assign "x" (IntLit 4)] (IntLit 4) IntT]
                    `shouldBe`
                    Right [DefFun "f" [] [Decl "x" IntT (IntLit 3, IntT), Assign "x" (IntLit 4, IntT)] (IntLit 4, IntT) IntT]
            it "fails if the return type is incorrect" do
                runTypecheck [DefFun "f" [] [Decl "x" IntT (IntLit 3), Assign "x" (IntLit 4)] (BoolLit True) IntT]
                    `shouldBe`
                    Left (WrongReturnType "f" IntT BoolT)
            it "propagates errors in the body" do
                runTypecheck [DefFun "f" [] [Decl "x" IntT (IntLit 3), Assign "x" (BoolLit False)] (IntLit 3) IntT]
                    `shouldBe`
                    Left (WrongAssignType "x" IntT BoolT)
            it "propagates errors in the last expression" do
                runTypecheck [DefFun "f" [] [Decl "x" IntT (IntLit 5)] (FCall "doesNotExist" []) IntT]
                    `shouldBe`
                    Left (FunctionDoesNotExist "doesNotExist")
            it "makes its local variables available in the last expression" do
                runTypecheck [DefFun "f" [] [Decl "x" IntT (IntLit 3)] (Var "x") IntT]
                    `shouldBe`
                    Right [DefFun "f" [] [Decl "x" IntT (IntLit 3, IntT)] (Var "x", IntT) IntT]
            it "is available in its body (allows for recursion)" do
                runTypecheck [DefFun "f" [] [Decl "x" IntT (FCall "f" [])] (Var "x") IntT]
                    `shouldBe`
                    Right [DefFun "f" [] [Decl "x" IntT (FCall "f" [], IntT)] (Var "x", IntT) IntT]
            it "has the correct recursive return type" do
                runTypecheck [DefFun "f" [] [Decl "x" BoolT (FCall "f" [])] (Var "x") IntT]
                    `shouldBe`
                    Left (WrongDeclType "f" BoolT IntT)
        --TODO

emptyTCState :: TCState
emptyTCState = TCState mempty mempty mempty

runTypecheck :: [Statement 'Untyped] -> Either TypeError [Statement 'Typed]
runTypecheck = fmap snd . runTypecheck' emptyTCState

runTypecheck' :: TCState -> [Statement 'Untyped] -> Either TypeError (TCState, [Statement 'Typed])
runTypecheck' tcstate = run . runError . runState tcstate . traverse typecheck

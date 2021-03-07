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
            runTypecheck [DeclU "x" (Just IntT) (IntLitU 5)] `shouldSatisfy` isRight
            runTypecheck [DeclU "y" (Just BoolT) (BoolLitU True)] `shouldSatisfy` isRight
        it "returns a typed, otherwise unchanged statement with correct types for the expression" do
            runTypecheck [DeclU "x" (Just IntT) (IntLitU 23)] 
                `shouldBe` 
                Right [DeclT "x" (Just IntT) (IntLitT 23)]
            runTypecheck [DeclU "x" (Just BoolT) (BoolLitU False)] 
                `shouldBe` 
                Right [DeclT "x" (Just BoolT) (BoolLitT False)]
        it "fails for incorrect types" do
            runTypecheck [DeclU "x" (Just IntT) (BoolLitU True)] `shouldSatisfy` isLeft
            runTypecheck [DeclU "x" (Just BoolT) (IntLitU 2)] `shouldSatisfy` isLeft
        it "gives correct error messages" do
            runTypecheck [DeclU "x" (Just IntT) (BoolLitU True)] `shouldBe` Left (WrongDeclType "x" IntT BoolT)
            runTypecheck [DeclU "y" (Just BoolT) (IntLitU 45)] `shouldBe` Left (WrongDeclType "y" BoolT IntT)
        it "propagates errors" do
            runTypecheck [DeclU "x" (Just IntT) (FCallU "doesNotExist" [])] `shouldBe` Left (FunctionDoesNotExist "doesNotExist")
            runTypecheck [DeclU "y" (Just BoolT) (FCallU "doesNotExist" [])] `shouldBe` Left (FunctionDoesNotExist "doesNotExist")
    describe "Assign" do
        context "when a variable was previosly declared" do
            it "does not fail for correct types" do
                runTypecheck [DeclU "x" (Just IntT) (IntLitU 5), AssignU "x" (IntLitU 7)] `shouldSatisfy` isRight
                runTypecheck [DeclU "x" (Just BoolT) (BoolLitU True), AssignU "x" (BoolLitU False)] `shouldSatisfy` isRight
            it "returns a typed, otherwise unchanged statements with correct types for the expressions" do
                runTypecheck [DeclU "x" (Just IntT) (IntLitU 5), AssignU "x" (IntLitU 7)]
                    `shouldBe`
                    Right [DeclT "x" (Just IntT) (IntLitT 5), AssignT "x" (IntLitT 7)]
            it "fails for incorrect types" do
                runTypecheck [DeclU "x" (Just IntT) (IntLitU 5), AssignU "x" (BoolLitU False)]
                    `shouldSatisfy`
                    isLeft
                runTypecheck [DeclU "x" (Just BoolT) (BoolLitU True), AssignU "x" (IntLitU 2)]
                    `shouldSatisfy`
                    isLeft
            it "gives correct error messages" do
                runTypecheck [DeclU "x" (Just IntT) (IntLitU 5), AssignU "x" (BoolLitU False)]
                    `shouldBe`
                    Left (WrongAssignType "x" IntT BoolT)
                runTypecheck [DeclU "x" (Just BoolT) (BoolLitU True), AssignU "x" (IntLitU 2)]
                    `shouldBe`
                    Left (WrongAssignType "x" BoolT IntT)
            it "propagates errors in the assigned expression" do
                runTypecheck [DeclU "x" (Just IntT) (IntLitU 5), AssignU "x" (FCallU "doesNotExist" [])]
                    `shouldBe`
                    Left (FunctionDoesNotExist "doesNotExist")
                runTypecheck [DeclU "x" (Just BoolT) (BoolLitU False), AssignU "x" (FCallU "doesNotExist" [])]
                    `shouldBe`
                    Left (FunctionDoesNotExist "doesNotExist")
        context "with a free variable" do
            it "fails" do
                runTypecheck [AssignU "x" (IntLitU 3)] `shouldSatisfy` isLeft
            it "creates an apropriate error message" do
                runTypecheck [AssignU "x" (IntLitU 3)] `shouldBe` Left (VarDoesNotExist "x")
            it "fails before any erors in the expression" do
                runTypecheck [AssignU "x" (FCallU "doesNotExist" [])] `shouldBe` Left (VarDoesNotExist "x")

    describe "DefVoid" do
        context "without parameters" do
            it "does not fail if the body is correct" do
                runTypecheck [DefVoidU "f" [] [DeclU "x" (Just IntT) (IntLitU 5)]] `shouldSatisfy` isRight
            it "gives back a correctly typed, otherwise unchanged statement" do
                runTypecheck [DefVoidU "f" [] [DeclU "x" (Just IntT) (IntLitU 5), AssignU "x" (IntLitU 4)]]
                    `shouldBe`
                    Right [DefVoid void_ "f" [] [DeclT "x" (Just IntT) (IntLitT 5), AssignT "x" (IntLitT 4)]]
            it "propagates errors in the body" do
                runTypecheck [DefVoidU "f" [] [DeclU "x" (Just IntT) (IntLitU 5), AssignU "x" (BoolLitU False)]]
                    `shouldBe`
                    Left (WrongAssignType "x" IntT BoolT)
            it "is available inside the body (allows for recursion)" do
                runTypecheck [DefVoidU "f" [] [CallFunU "f" []]]
                    `shouldBe`
                    Right [DefVoidT "f" [] [CallFunT "f" []]]
        context "with parameters" do
            it "makes its parameters available in its body" do
                runTypecheck [DefVoidU "f" [("x", IntT)] [DeclU "y" (Just IntT) (VarU "x")]]
                    `shouldBe`
                    Right [DefVoidT "f" [("x", IntT)] [DeclT "y" (Just IntT) (VarT IntT "x")]]
            it "has the correct type in its body (recursion)" do
                runTypecheck [DefVoidU "f" [("x", IntT), ("y", BoolT)] [CallFunU "f" [VarU "x", VarU "y"]]]
                    `shouldBe`
                    Right [DefVoidT "f" [("x", IntT), ("y", BoolT)] [CallFunT "f" [VarT IntT "x", VarT IntT "y"]]]
    describe "DefFun" do
        context "without parameters" do
            it "does not fail if the body and the return type are correct" do
                runTypecheck [DefFunU "f" [] [DeclU "x" (Just IntT) (IntLitU 3), AssignU "x" (IntLitU 4)] (IntLitU 4) IntT]
                    `shouldBe`
                    Right [DefFunT "f" [] [DeclT "x" (Just IntT) (IntLitT 3), AssignT "x" (IntLitT 4)] (IntLitT  4) IntT]
            it "fails if the return type is incorrect" do
                runTypecheck [DefFunU "f" [] [DeclU "x" (Just IntT) (IntLitU 3), AssignU "x" (IntLitU 4)] (BoolLitU True) IntT]
                    `shouldBe`
                    Left (WrongReturnType "f" IntT BoolT)
            it "propagates errors in the body" do
                runTypecheck [DefFunU "f" [] [DeclU "x" (Just IntT) (IntLitU 3), AssignU "x" (BoolLitU False)] (IntLitU 3) IntT]
                    `shouldBe`
                    Left (WrongAssignType "x" IntT BoolT)
            it "propagates errors in the last expression" do
                runTypecheck [DefFunU "f" [] [DeclU "x" (Just IntT) (IntLitU 5)] (FCallU "doesNotExist" []) IntT]
                    `shouldBe`
                    Left (FunctionDoesNotExist "doesNotExist")
            it "makes its local variables available in the last expression" do
                runTypecheck [DefFunU "f" [] [DeclU "x" (Just IntT) (IntLitU 3)] (VarU "x") IntT]
                    `shouldBe`
                    Right [DefFunT "f" [] [DeclT "x" (Just IntT) (IntLitT 3)] (VarT IntT "x") IntT]
            it "is available in its body (allows for recursion)" do
                runTypecheck [DefFunU "f" [] [DeclU "x" (Just IntT) (FCallU "f" [])] (VarU "x") IntT]
                    `shouldBe`
                    Right [DefFunT "f" [] [DeclT "x" (Just IntT) (FCallT IntT "f" [])] (VarT IntT "x") IntT]
            it "has the correct recursive return type" do
                runTypecheck [DefFunU "f" [] [DeclU "x" (Just BoolT) (FCallU "f" [])] (VarU "x") IntT]
                    `shouldBe`
                    Left (WrongDeclType "f" BoolT IntT)
        --TODO

emptyTCState :: TCState
emptyTCState = TCState mempty mempty mempty

runTypecheck :: [Statement 'Unaltered] -> Either TypeError [Statement 'Typed]
runTypecheck = fmap snd . runTypecheck' emptyTCState

runTypecheck' :: TCState -> [Statement 'Unaltered] -> Either TypeError (TCState, [Statement 'Typed])
runTypecheck' tcstate = run . runError . runState tcstate . traverse typecheck

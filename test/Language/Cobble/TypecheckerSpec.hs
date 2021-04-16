module Language.Cobble.TypecheckerSpec where

import Language.Cobble.Prelude
import Language.Cobble.Typechecker as TC
import Language.Cobble.Types

import Test.Hspec as S

spec :: Spec
spec = do
    let l = dummyLex
    describe "Decl" do
        it "does not fail for correct types" do
            runTypecheck [Decl () l "x" (Just intT) (IntLit () l 5)] `shouldSatisfy` isRight
            runTypecheck [Decl () l "y" (Just boolT) (BoolLit () l True)] `shouldSatisfy` isRight
        it "returns a typed, otherwise unchanged statement with correct types for the expression" do
            runTypecheck [Decl () l "x" (Just intT) (IntLit () l 23)] 
                `shouldBe` 
                Right [Decl () l "x" (Just intT) (IntLit () l 23)]
            runTypecheck [Decl () l "x" (Just boolT) (BoolLit () l False)] 
                `shouldBe` 
                Right [Decl () l "x" (Just boolT) (BoolLit () l False)]
        it "fails for incorrect types" do
            runTypecheck [Decl () l "x" (Just intT) (BoolLit () l True)] `shouldSatisfy` isLeft
            runTypecheck [Decl () l "x" (Just boolT) (IntLit () l 2)] `shouldSatisfy` isLeft
        it "gives correct error messages" do
            runTypecheck [Decl () l "x" (Just intT) (BoolLit () l True)] `shouldBe` Left (WrongDeclType l "x" intT boolT)
            runTypecheck [Decl () l "y" (Just boolT) (IntLit () l 45)] `shouldBe` Left (WrongDeclType l "y" boolT intT)
        it "propagates errors" do
            runTypecheck [Decl () l "x" (Just intT) (FCall () l "doesNotExist" [])] `shouldBe` Left (FunctionDoesNotExist l "doesNotExist")
            runTypecheck [Decl () l "y" (Just boolT) (FCall () l "doesNotExist" [])] `shouldBe` Left (FunctionDoesNotExist l "doesNotExist")
    describe "Assign" do
        context "when a variable was previosly declared" do
            it "does not fail for correct types" do
                runTypecheck [Decl () l "x" (Just intT) (IntLit () l 5), Assign () l "x" (IntLit () l 7)] `shouldSatisfy` isRight
                runTypecheck [Decl () l "x" (Just boolT) (BoolLit () l True), Assign () l "x" (BoolLit () l False)] `shouldSatisfy` isRight
            it "returns a typed, otherwise unchanged statements with correct types for the expressions" do
                runTypecheck [Decl () l "x" (Just intT) (IntLit () l 5), Assign () l "x" (IntLit () l 7)]
                    `shouldBe`
                    Right [Decl () l "x" (Just intT) (IntLit () l 5), Assign () l "x" (IntLit () l 7)]
            it "fails for incorrect types" do
                runTypecheck [Decl () l "x" (Just intT) (IntLit () l 5), Assign () l "x" (BoolLit () l False)]
                    `shouldSatisfy`
                    isLeft
                runTypecheck [Decl () l "x" (Just boolT) (BoolLit () l True), Assign () l "x" (IntLit () l 2)]
                    `shouldSatisfy`
                    isLeft
            it "gives correct error messages" do
                runTypecheck [Decl () l "x" (Just intT) (IntLit () l 5), Assign () l "x" (BoolLit () l False)]
                    `shouldBe`
                    Left (WrongAssignType l "x" intT boolT)
                runTypecheck [Decl () l "x" (Just boolT) (BoolLit () l True), Assign () l "x" (IntLit () l 2)]
                    `shouldBe`
                    Left (WrongAssignType l "x" boolT intT)
            it "propagates errors in the assigned expression" do
                runTypecheck [Decl () l "x" (Just intT) (IntLit () l 5), Assign () l "x" (FCall () l "doesNotExist" [])]
                    `shouldBe`
                    Left (FunctionDoesNotExist l "doesNotExist")
                runTypecheck [Decl () l "x" (Just boolT) (BoolLit () l False), Assign () l "x" (FCall () l "doesNotExist" [])]
                    `shouldBe`
                    Left (FunctionDoesNotExist l "doesNotExist")
        context "with a free variable" do
            it "fails" do
                runTypecheck [Assign () l "x" (IntLit () l 3)] `shouldSatisfy` isLeft
            it "creates an apropriate error message" do
                runTypecheck [Assign () l "x" (IntLit () l 3)] `shouldBe` Left (VarDoesNotExist l "x")
            it "fails before any erors in the expression" do
                runTypecheck [Assign () l "x" (FCall () l "doesNotExist" [])] `shouldBe` Left (VarDoesNotExist l "x")

    describe "DefVoid" do
        context "without parameters" do
            it "does not fail if the body is correct" do
                runTypecheck [DefVoid () l "f" [] [Decl () l "x" (Just intT) (IntLit () l 5)]] `shouldSatisfy` isRight
            it "gives back a correctly typed, otherwise unchanged statement" do
                runTypecheck [DefVoid () l "f" [] [Decl () l "x" (Just intT) (IntLit () l 5), Assign () l "x" (IntLit () l 4)]]
                    `shouldBe`
                    Right [DefVoid () l "f" [] [Decl () l "x" (Just intT) (IntLit () l 5), Assign () l "x" (IntLit () l 4)]]
            it "propagates errors in the body" do
                runTypecheck [DefVoid () l "f" [] [Decl () l "x" (Just intT) (IntLit () l 5), Assign () l "x" (BoolLit () l False)]]
                    `shouldBe`
                    Left (WrongAssignType l "x" intT boolT)
            it "is available inside the body (allows for recursion)" do
                runTypecheck [DefVoid () l "f" [] [CallFun () l "f" []]]
                    `shouldBe`
                    Right [DefVoid () l "f" [] [CallFun () l "f" []]]
        context "with parameters" do
            it "makes its parameters available in its body" do
                runTypecheck [DefVoid () l "f" [("x", intT)] [Decl () l "y" (Just intT) (Var () l "x")]]
                    `shouldBe`
                    Right [DefVoid () l "f" [("x", intT)] [Decl () l "y" (Just intT) (Var intT l "x")]]
            it "has the correct type in its body (recursion)" do
                runTypecheck [DefVoid () l "f" [("x", intT), ("y", boolT)] [CallFun () l "f" [Var () l "x", Var () l "y"]]]
                    `shouldBe`
                    Right [DefVoid () l "f" [("x", intT), ("y", boolT)] [CallFun () l "f" [Var intT l "x", Var boolT l "y"]]]
    describe "DefFun" do
        context "without parameters" do
            it "does not fail if the body and the return type are correct" do
                runTypecheck [DefFun () l "f" [] [Decl () l "x" (Just intT) (IntLit () l 3), Assign () l "x" (IntLit () l 4)] (IntLit () l 4) intT]
                    `shouldBe`
                    Right [DefFun () l "f" [] [Decl () l "x" (Just intT) (IntLit () l 3), Assign () l "x" (IntLit () l 4)] (IntLit () l 4) intT]
            it "fails if the return type is incorrect" do
                runTypecheck [DefFun () l "f" [] [Decl () l "x" (Just intT) (IntLit () l 3), Assign () l "x" (IntLit () l 4)] (BoolLit () l True) intT]
                    `shouldBe`
                    Left (WrongReturnType l "f" intT boolT)
            it "propagates errors in the body" do
                runTypecheck [DefFun () l "f" [] [Decl () l "x" (Just intT) (IntLit () l 3), Assign () l "x" (BoolLit () l False)] (IntLit () l 3) intT]
                    `shouldBe`
                    Left (WrongAssignType l "x" intT boolT)
            it "propagates errors in the last expression" do
                runTypecheck [DefFun () l "f" [] [Decl () l "x" (Just intT) (IntLit () l 5)] (FCall () l "doesNotExist" []) intT]
                    `shouldBe`
                    Left (FunctionDoesNotExist l "doesNotExist")
            it "makes its local variables available in the last expression" do
                runTypecheck [DefFun () l "f" [] [Decl () l "x" (Just intT) (IntLit () l 3)] (Var () l "x") intT]
                    `shouldBe`
                    Right [DefFun () l "f" [] [Decl () l "x" (Just intT) (IntLit () l 3)] (Var intT l "x") intT]
            it "is available in its body (allows for recursion)" do
                runTypecheck [DefFun () l "f" [] [Decl () l "x" (Just intT) (FCall () l "f" [])] (Var () l "x") intT]
                    `shouldBe`
                    Right [DefFun () l "f" [] [Decl () l "x" (Just intT) (FCall intT l "f" [])] (Var intT l "x") intT]
            it "has the correct recursive return type" do
                runTypecheck [
                    DefFun () l "f" []  [
                        Decl () l "x" (Just boolT) (FCall () l "f" [])
                      ] (IntLit () l 42) intT
                    ]
                    `shouldBe`
                    Left (WrongDeclType l "x" boolT intT)

    describe "CallFun" do
        context "void functions" do
            it "does not fail if called with the correct arguments" do
                runTypecheck [
                        DefVoid () l "f" [("x", intT)] []
                    ,   CallFun () l "f" [IntLit () l 5]
                    ]
                    `shouldBe`
                    Right [
                        DefVoid () l "f" [("x", intT)] []
                    ,   CallFun () l "f" [IntLit () l 5]
                    ]
        context "regular functions" do
            it "does not fail if called with the correct arguments" do
                runTypecheck [
                        DefFun () l "f" [("x", intT)] [] (IntLit () l 10) intT
                    ,   CallFun () l "f" [IntLit () l 34]
                    ]
                    `shouldBe` Right [
                        DefFun () l "f" [("x", intT)] [] (IntLit () l 10) intT
                    ,   CallFun () l "f" [IntLit () l 34]                    
                    ]

emptyTCState :: TCState
emptyTCState = TCState mempty mempty mempty

runTypecheck :: [Statement 'Typecheck] -> Either TypeError [Statement NextPass]
runTypecheck = fmap snd . runTypecheck' emptyTCState

runTypecheck' :: TCState -> [Statement 'Typecheck] -> Either TypeError (TCState, [Statement NextPass])
runTypecheck' tcstate = run . runError . runState tcstate . traverse typecheck

dummyLex :: LexInfo
dummyLex = LexInfo 0 0 "DUMMY"

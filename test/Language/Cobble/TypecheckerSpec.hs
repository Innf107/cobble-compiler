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
            runTypecheck [DeclU l "x" (Just intT) (IntLitU l 5)] `shouldSatisfy` isRight
            runTypecheck [DeclU l "y" (Just boolT) (BoolLitU l True)] `shouldSatisfy` isRight
        it "returns a typed, otherwise unchanged statement with correct types for the expression" do
            runTypecheck [DeclU l "x" (Just intT) (IntLitU l 23)] 
                `shouldBe` 
                Right [DeclT l "x" (Just intT) (IntLitT l 23)]
            runTypecheck [DeclU l "x" (Just boolT) (BoolLitU l False)] 
                `shouldBe` 
                Right [DeclT l "x" (Just boolT) (BoolLitT l False)]
        it "fails for incorrect types" do
            runTypecheck [DeclU l "x" (Just intT) (BoolLitU l True)] `shouldSatisfy` isLeft
            runTypecheck [DeclU l "x" (Just boolT) (IntLitU l 2)] `shouldSatisfy` isLeft
        it "gives correct error messages" do
            runTypecheck [DeclU l "x" (Just intT) (BoolLitU l True)] `shouldBe` Left (WrongDeclType l "x" intT boolT)
            runTypecheck [DeclU l "y" (Just boolT) (IntLitU l 45)] `shouldBe` Left (WrongDeclType l "y" boolT intT)
        it "propagates errors" do
            runTypecheck [DeclU l "x" (Just intT) (FCallU l "doesNotExist" [])] `shouldBe` Left (FunctionDoesNotExist l "doesNotExist")
            runTypecheck [DeclU l "y" (Just boolT) (FCallU l "doesNotExist" [])] `shouldBe` Left (FunctionDoesNotExist l "doesNotExist")
    describe "Assign" do
        context "when a variable was previosly declared" do
            it "does not fail for correct types" do
                runTypecheck [DeclU l "x" (Just intT) (IntLitU l 5), AssignU l "x" (IntLitU l 7)] `shouldSatisfy` isRight
                runTypecheck [DeclU l "x" (Just boolT) (BoolLitU l True), AssignU l "x" (BoolLitU l False)] `shouldSatisfy` isRight
            it "returns a typed, otherwise unchanged statements with correct types for the expressions" do
                runTypecheck [DeclU l "x" (Just intT) (IntLitU l 5), AssignU l "x" (IntLitU l 7)]
                    `shouldBe`
                    Right [DeclT l "x" (Just intT) (IntLitT l 5), AssignT l "x" (IntLitT l 7)]
            it "fails for incorrect types" do
                runTypecheck [DeclU l "x" (Just intT) (IntLitU l 5), AssignU l "x" (BoolLitU l False)]
                    `shouldSatisfy`
                    isLeft
                runTypecheck [DeclU l "x" (Just boolT) (BoolLitU l True), AssignU l "x" (IntLitU l 2)]
                    `shouldSatisfy`
                    isLeft
            it "gives correct error messages" do
                runTypecheck [DeclU l "x" (Just intT) (IntLitU l 5), AssignU l "x" (BoolLitU l False)]
                    `shouldBe`
                    Left (WrongAssignType l "x" intT boolT)
                runTypecheck [DeclU l "x" (Just boolT) (BoolLitU l True), AssignU l "x" (IntLitU l 2)]
                    `shouldBe`
                    Left (WrongAssignType l "x" boolT intT)
            it "propagates errors in the assigned expression" do
                runTypecheck [DeclU l "x" (Just intT) (IntLitU l 5), AssignU l "x" (FCallU l "doesNotExist" [])]
                    `shouldBe`
                    Left (FunctionDoesNotExist l "doesNotExist")
                runTypecheck [DeclU l "x" (Just boolT) (BoolLitU l False), AssignU l "x" (FCallU l "doesNotExist" [])]
                    `shouldBe`
                    Left (FunctionDoesNotExist l "doesNotExist")
        context "with a free variable" do
            it "fails" do
                runTypecheck [AssignU l "x" (IntLitU l 3)] `shouldSatisfy` isLeft
            it "creates an apropriate error message" do
                runTypecheck [AssignU l "x" (IntLitU l 3)] `shouldBe` Left (VarDoesNotExist l "x")
            it "fails before any erors in the expression" do
                runTypecheck [AssignU l "x" (FCallU l "doesNotExist" [])] `shouldBe` Left (VarDoesNotExist l "x")

    describe "DefVoid" do
        context "without parameters" do
            it "does not fail if the body is correct" do
                runTypecheck [DefVoidU l "f" [] [DeclU l "x" (Just intT) (IntLitU l 5)]] `shouldSatisfy` isRight
            it "gives back a correctly typed, otherwise unchanged statement" do
                runTypecheck [DefVoidU l "f" [] [DeclU l "x" (Just intT) (IntLitU l 5), AssignU l "x" (IntLitU l 4)]]
                    `shouldBe`
                    Right [DefVoidT l "f" [] [DeclT l "x" (Just intT) (IntLitT l 5), AssignT l "x" (IntLitT l 4)]]
            it "propagates errors in the body" do
                runTypecheck [DefVoidU l "f" [] [DeclU l "x" (Just intT) (IntLitU l 5), AssignU l "x" (BoolLitU l False)]]
                    `shouldBe`
                    Left (WrongAssignType l "x" intT boolT)
            it "is available inside the body (allows for recursion)" do
                runTypecheck [DefVoidU l "f" [] [CallFunU l "f" []]]
                    `shouldBe`
                    Right [DefVoidT l "f" [] [CallFunT l "f" []]]
        context "with parameters" do
            it "makes its parameters available in its body" do
                runTypecheck [DefVoidU l "f" [("x", intT)] [DeclU l "y" (Just intT) (VarU l "x")]]
                    `shouldBe`
                    Right [DefVoidT l "f" [("x", intT)] [DeclT l "y" (Just intT) (VarT intT l "x")]]
            it "has the correct type in its body (recursion)" do
                runTypecheck [DefVoidU l "f" [("x", intT), ("y", boolT)] [CallFunU l "f" [VarU l "x", VarU l "y"]]]
                    `shouldBe`
                    Right [DefVoidT l "f" [("x", intT), ("y", boolT)] [CallFunT l "f" [VarT intT l "x", VarT boolT l "y"]]]
    describe "DefFun" do
        context "without parameters" do
            it "does not fail if the body and the return type are correct" do
                runTypecheck [DefFunU l "f" [] [DeclU l "x" (Just intT) (IntLitU l 3), AssignU l "x" (IntLitU l 4)] (IntLitU l 4) intT]
                    `shouldBe`
                    Right [DefFunT l "f" [] [DeclT l "x" (Just intT) (IntLitT l 3), AssignT l "x" (IntLitT l 4)] (IntLitT l 4) intT]
            it "fails if the return type is incorrect" do
                runTypecheck [DefFunU l "f" [] [DeclU l "x" (Just intT) (IntLitU l 3), AssignU l "x" (IntLitU l 4)] (BoolLitU l True) intT]
                    `shouldBe`
                    Left (WrongReturnType l "f" intT boolT)
            it "propagates errors in the body" do
                runTypecheck [DefFunU l "f" [] [DeclU l "x" (Just intT) (IntLitU l 3), AssignU l "x" (BoolLitU l False)] (IntLitU l 3) intT]
                    `shouldBe`
                    Left (WrongAssignType l "x" intT boolT)
            it "propagates errors in the last expression" do
                runTypecheck [DefFunU l "f" [] [DeclU l "x" (Just intT) (IntLitU l 5)] (FCallU l "doesNotExist" []) intT]
                    `shouldBe`
                    Left (FunctionDoesNotExist l "doesNotExist")
            it "makes its local variables available in the last expression" do
                runTypecheck [DefFunU l "f" [] [DeclU l "x" (Just intT) (IntLitU l 3)] (VarU l "x") intT]
                    `shouldBe`
                    Right [DefFunT l "f" [] [DeclT l "x" (Just intT) (IntLitT l 3)] (VarT intT l "x") intT]
            it "is available in its body (allows for recursion)" do
                runTypecheck [DefFunU l "f" [] [DeclU l "x" (Just intT) (FCallU l "f" [])] (VarU l "x") intT]
                    `shouldBe`
                    Right [DefFunT l "f" [] [DeclT l "x" (Just intT) (FCallT intT l "f" [])] (VarT intT l "x") intT]
            it "has the correct recursive return type" do
                runTypecheck [
                    DefFunU l "f" []  [
                        DeclU l "x" (Just boolT) (FCallU l "f" [])
                      ] (IntLitU l 42) intT
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

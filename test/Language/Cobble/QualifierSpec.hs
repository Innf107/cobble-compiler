{-# LANGUAGE NoImplicitPrelude, DataKinds, BlockArguments, OverloadedStrings #-}
module Language.Cobble.QualifierSpec where

import Language.Cobble.Prelude
import Language.Cobble.Qualifier as Q
import Language.Cobble.Types

import Test.Hspec as S

spec :: Spec
spec = do
    describe "qualifyStatement" do
        describe "CallFun" do
            it "finds the function name in the surrounding context" do
                testQual [Scope "Mod2" [] ["f"]] (qualifyStatement (
                    CallFun () dli "f" []
                    ))
                    `shouldBe`
                    Right (CallFun () dli "Mod2.f" [])
            it "does not use a type name" do
                testQual [Scope "Mod2" ["f"] []] (qualifyStatement (
                    CallFun () dli "f" []
                    ))
                    `shouldBe`
                    Left (NameNotFound dli "f")
            it "throws an error if the type is not found" do
                testQual [Scope "Mod2" [] ["g"]] (qualifyStatement (
                    CallFun () dli "f" []
                    ))
                    `shouldBe`
                    Left (NameNotFound dli "f")
            it "does not start a new qualification context for its parameters" do
                testQual [Scope "Mod2" [] ["f", "x"], Scope "Mod1" [] ["y"]] (qualifyStatement (
                    CallFun () dli "f" [Var () dli "x", Var () dli "y"]
                    ))
                    `shouldBe`
                    Right (CallFun () dli "Mod2.f" [Var () dli "Mod2.x", Var () dli "Mod1.y"])
        describe "DefVoid" do
            it "carries over the previous qualification context" do
                testQual [] (qualifyStatement (
                    DefVoid () dli "f" [("x", IntT)] []
                    ))
                    `shouldBe`
                    Right (DefVoid () dli "Mod1.f" [("Mod1.@fun_f.x", IntT)] [])
            it "starts a new qualification context in its body" do
                testQual [] (qualifyStatement (
                    DefVoid () dli "f" [] [Decl () dli "x" Nothing (IntLit () dli 5)]
                    ))
                    `shouldBe`
                    Right (DefVoid () dli "Mod1.f" [] [Decl () dli "Mod1.@fun_f.x" Nothing (IntLit () dli 5)])
            it "is added to the names in the current Scope" do
                testQual [] (traverse qualifyStatement [
                      DefVoid () dli "f" [] [CallFun () dli "f" []]
                    , CallFun () dli "f" []
                    ])
                    `shouldBe`
                    Right [
                      DefVoid () dli "Mod1.f" [] [CallFun () dli "Mod1.f" []]
                    , CallFun () dli "Mod1.f" []
                    ]
            it "keeps its parameters in scope for the body" do
                testQual [] (qualifyStatement (
                    DefVoid () dli "f" [("x", IntT)] [Decl () dli "y" Nothing (Var () dli "x")]
                    ))
                    `shouldBe`
                    Right (DefVoid () dli "Mod1.f" [("Mod1.@fun_f.x", IntT)] [Decl () dli "Mod1.@fun_f.y" Nothing (Var () dli "Mod1.@fun_f.x")])
        describe "DefFun" do
            it "carries over the previous qualification context" do
                testQual [] (qualifyStatement (
                    DefFun () dli "f" [("x", IntT)] [] (IntLit () dli 5) IntT
                    ))
                    `shouldBe`
                    Right (DefFun () dli "Mod1.f" [("Mod1.@fun_f.x", IntT)] [] (IntLit () dli 5) IntT)
            it "starts a new qualification context in its body" do
                testQual [] (qualifyStatement (
                    DefFun () dli "f" [] [Decl () dli "x" Nothing (IntLit () dli 5)] (IntLit () dli 5) IntT
                    ))
                    `shouldBe`
                    Right (DefFun () dli "Mod1.f" [] [Decl () dli "Mod1.@fun_f.x" Nothing (IntLit () dli 5)] (IntLit () dli 5) IntT)
            it "keeps its parameters in scope for the body and return expression" do
                testQual [] (qualifyStatement (
                    DefFun () dli "f" [("x", IntT)] [Decl () dli "y" Nothing (Var () dli "x")] (Var () dli "x") IntT
                    ))
                    `shouldBe`
                    Right (DefFun () dli "Mod1.f" [("Mod1.@fun_f.x", IntT)] [Decl () dli "Mod1.@fun_f.y" Nothing (Var () dli "Mod1.@fun_f.x")] (Var () dli "Mod1.@fun_f.x") IntT)
            it "starts a new qualification context in its return expression" do
                testQual [] (qualifyStatement (
                    DefFun () dli "f" [("x", IntT)] [] (Var () dli "x") IntT
                    ))
                    `shouldBe`
                    Right (DefFun () dli "Mod1.f" [("Mod1.@fun_f.x", IntT)] [] (Var () dli "Mod1.@fun_f.x") IntT)
            it "keeps variables from its body in scope for the return expression" do
                testQual [] (qualifyStatement (
                    DefFun () dli "f" [] [Decl () dli "y" Nothing (IntLit () dli 5)] (Var () dli "y") IntT
                    ))
                    `shouldBe`
                    Right (DefFun () dli "Mod1.f" [] [Decl () dli "Mod1.@fun_f.y" Nothing (IntLit () dli 5)] (Var () dli "Mod1.@fun_f.y") IntT)
            it "is added to the names in the current Scope" do
                testQual [] (traverse qualifyStatement [
                      DefFun() dli "f" [] [CallFun () dli "f" []] (FCall () dli "f" []) IntT
                    , CallFun () dli "f" []
                    ])
                    `shouldBe`
                    Right [
                      DefFun () dli "Mod1.f" [] [CallFun () dli "Mod1.f" []] (FCall () dli "Mod1.f" []) IntT
                    , CallFun () dli "Mod1.f" []
                    ]
        describe "Decl" do
            it "is added to the names in the current scope" do
                testQual [] (traverse qualifyStatement [
                      Decl () dli "x" Nothing (IntLit () dli 5)
                    , Decl () dli "y" Nothing (Var () dli "x")
                    ])
                    `shouldBe`
                    Right [
                      Decl () dli "Mod1.x" Nothing (IntLit () dli 5)
                    , Decl () dli "Mod1.y" Nothing (Var () dli "Mod1.x")
                    ]
        describe "Assign" do
            it "finds the variable from the surrounding environment" do
                testQual [] (traverse qualifyStatement [
                      Decl () dli "x" (Just IntT) (IntLit () dli 5)
                    , Assign () dli "x" (Var () dli "x")
                    ])
                    `shouldBe`
                    Right [
                      Decl () dli "Mod1.x" (Just IntT) (IntLit () dli 5)
                    , Assign () dli "Mod1.x" (Var () dli "Mod1.x")
                    ]
        --TODO
        describe "While" do
            it "creates a new scope for its body" do
                testQual [] (traverse qualifyStatement [
                      Decl () dli "x" (Just IntT) (IntLit () dli 5)
                    , While () dli (BoolLit () dli True) [
                        Decl () dli "x" (Just IntT) (IntLit () dli 6)
                      , Assign () dli "x" (Var () dli "x")
                      ]
                    , Assign () dli "x" (Var () dli "x")
                    ])
                    `shouldBe`
                    Right [
                      Decl () dli "Mod1.x" (Just IntT) (IntLit () dli 5)
                    , While () dli (BoolLit () dli True) [
                        Decl () dli "Mod1.@while1.x" (Just IntT) (IntLit () dli 6)
                      , Assign () dli "Mod1.@while1.x" (Var () dli "Mod1.@while1.x")
                      ]
                    , Assign () dli "Mod1.x" (Var () dli "Mod1.x")
                    ]
        describe "DefStruct" do
            it "correctly qualifies its field types" do
                testQual [] (qualifyStatement (DefStruct () dli "SomeStruct" [("x", IntT)]))
                    `shouldBe`
                    Right (DefStruct () dli "Mod1.SomeStruct" [("Mod1.SomeStruct.x", IntT)])
            it "is available inside its field types (structural recursion)" do -- TODO: Direct structural recursion might have to be detected
                testQual [] (qualifyStatement (DefStruct () dli "SomeStruct" [("f", StructT "SomeStruct")]))
                    `shouldBe`
                    Right (DefStruct () dli "Mod1.SomeStruct" [("Mod1.SomeStruct.f", StructT "Mod1.SomeStruct")])
            it "is added to the type names in the current scopes" do
                testQual [] (traverse qualifyStatement [
                      DefStruct () dli "SomeStruct" []
                    , Decl () dli "x" (Just (StructT "SomeStruct")) (IntLit () dli 42)
                    ])
                    `shouldBe`
                    Right [
                      DefStruct () dli "Mod1.SomeStruct" []
                    , Decl () dli "Mod1.x" (Just (StructT "Mod1.SomeStruct")) (IntLit () dli 42)
                    ]
            it "does not bleed into the outer scope" do
                testQual [] (traverse qualifyStatement [
                      DefVoid () dli "f" [] [
                        DefStruct () dli "SomeStruct" []
                      ]
                      , Decl () dli "x" (Just (StructT "SomeStruct")) (IntLit () dli 42)
                    ])
                    `shouldBe`
                    Left (TypeNotFound dli "SomeStruct")
    describe "qualifyExpr" do
        describe "FCall" do
            it "finds the function name from the outer scope" do
                testQual [Scope "Mod2" [] ["f"]] (qualifyExp (
                        FCall () dli "f" []
                    ))
                    `shouldBe`
                    Right (FCall () dli "Mod2.f" [])
            it "fully qualifies its arguments" do
                testQual [Scope "Mod2" [] ["f", "x"]] (qualifyExp (
                    FCall () dli "f" [(Var () dli "x"), (Var () dli "f")]
                    ))
                    `shouldBe`
                    Right (FCall () dli "Mod2.f" [Var () dli "Mod2.x", Var () dli "Mod2.f"])
        describe "IntLit" do
            it "is completely unaffected" do
                testQual [] (qualifyExp (IntLit () dli 42))
                    `shouldBe`
                    Right (IntLit () dli 42)
        describe "BoolLit" do
            it "is completely unaffected" do
                testQual [] (qualifyExp (BoolLit () dli True))
                    `shouldBe`
                    Right (BoolLit () dli True)
        describe "Var" do
            it "finds its name in the surrounding environment" do
                testQual [Scope "Mod2" [] ["x"]] (qualifyExp (Var () dli "x"))
                   `shouldBe`
                   Right (Var () dli "Mod2.x")
            it "throws an error if the name does not exist" do
                testQual [] (qualifyExp (Var () dli "x"))
                    `shouldBe`
                    Left (NameNotFound dli "x")


    describe "qualifyType" do
        pass

--TODO

dli :: LexInfo
dli = LexInfo 0 0 "Test"

testQual :: [Scope]
         -> Sem '[State Int, State [Scope], Error QualificationError] a
         -> Either QualificationError a
testQual s = run . runError . evalState (Scope "Mod1" [] [] : s) . evalState 0


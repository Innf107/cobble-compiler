module Language.Cobble.QualifierSpec where

import Language.Cobble.Prelude
import Language.Cobble.Qualifier as Q
import Language.Cobble.Types

import Test.Hspec as S

spec :: Spec
spec = do
    describe "qualifyStatement" do
        {-describe "CallFun" do
            it "finds the function name in the surrounding context" do
                testQual [Scope "Mod2" [] ["f"] mempty] (qualifyStatement (
                    CallFun () dli (Var () dli "f") []
                    ))
                    `shouldBe`
                    Right (CallFun () dli (Var () dli "Mod2.f") [])
            it "does not use a type name" do
                testQual [Scope "Mod2" ["f"] [] mempty] (qualifyStatement (
                    CallFun () dli (Var () dli "f") []
                    ))
                    `shouldBe`
                    Left (NameNotFound dli "f")
            it "throws an error if the type is not found" do
                testQual [Scope "Mod2" [] ["g"] mempty] (qualifyStatement (
                    CallFun () dli (Var () dli "f") []
                    ))
                    `shouldBe`
                    Left (NameNotFound dli "f")
            it "does not start a new qualification context for its parameters" do
                testQual [Scope "Mod2" [] ["f", "x"] mempty, Scope "Mod1" [] ["y"] mempty] (qualifyStatement (
                    CallFun () dli (Var () dli "f") [Var () dli "x", Var () dli "y"]
                    ))
                    `shouldBe`
                    Right (CallFun () dli (Var () dli "Mod2.f") [Var () dli "Mod2.x", Var () dli "Mod1.y"])
        describe "DefVoid" do
            it "carries over the previous qualification context" do
                testQual [] (qualifyStatement (
                    DefFun () dli "f" [("x", intT)] [] (UnitLit dli) unitT
                    ))
                    `shouldBe`
                    Right (DefFun () dli "Mod1.f" [("Mod1.-fun_f.x", intT)] [] (UnitLit dli) unitT)
            it "starts a new qualification context in its body" do
                testQual [] (qualifyStatement (
                    DefFun () dli "f" [] [Decl () dli "x" Nothing (IntLit () dli 5)] (UnitLit dli) unitT
                    ))
                    `shouldBe`
                    Right (DefFun () dli "Mod1.f" [] [Decl () dli "Mod1.-fun_f.x" Nothing (IntLit () dli 5)] (UnitLit dli) unitT)
            it "is added to the names in the current Scope" do
                testQual [] (traverse qualifyStatement [
                      DefFun () dli "f" [] [CallFun () dli (Var () dli "f") []] (UnitLit dli) unitT
                    , CallFun () dli (Var () dli "f") []
                    ])
                    `shouldBe`
                    Right [
                      DefFun () dli "Mod1.f" [] [CallFun () dli (Var () dli "Mod1.f") []] (UnitLit dli) unitT
                    , CallFun () dli (Var () dli "Mod1.f") []
                    ]
            it "keeps its parameters in scope for the body" do
                testQual [] (qualifyStatement (
                    DefFun () dli "f" [("x", intT)] [Decl () dli "y" Nothing (Var () dli "x")] (UnitLit dli) unitT
                    ))
                    `shouldBe`
                    Right (DefFun () dli "Mod1.f" [("Mod1.-fun_f.x", intT)] [Decl () dli "Mod1.-fun_f.y" Nothing (Var () dli "Mod1.-fun_f.x")] (UnitLit dli) unitT)
        describe "DefFun" do
            it "carries over the previous qualification context" do
                testQual [] (qualifyStatement (
                    DefFun () dli "f" [("x", intT)] [] (IntLit () dli 5) intT
                    ))
                    `shouldBe`
                    Right (DefFun () dli "Mod1.f" [("Mod1.-fun_f.x", intT)] [] (IntLit () dli 5) intT)
            it "starts a new qualification context in its body" do
                testQual [] (qualifyStatement (
                    DefFun () dli "f" [] [Decl () dli "x" Nothing (IntLit () dli 5)] (IntLit () dli 5) intT
                    ))
                    `shouldBe`
                    Right (DefFun () dli "Mod1.f" [] [Decl () dli "Mod1.-fun_f.x" Nothing (IntLit () dli 5)] (IntLit () dli 5) intT)
            it "keeps its parameters in scope for the body and return expression" do
                testQual [] (qualifyStatement (
                    DefFun () dli "f" [("x", intT)] [Decl () dli "y" Nothing (Var () dli "x")] (Var () dli "x") intT
                    ))
                    `shouldBe`
                    Right (DefFun () dli "Mod1.f" [("Mod1.-fun_f.x", intT)] [Decl () dli "Mod1.-fun_f.y" Nothing (Var () dli "Mod1.-fun_f.x")] (Var () dli "Mod1.-fun_f.x") intT)
            it "starts a new qualification context in its return expression" do
                testQual [] (qualifyStatement (
                    DefFun () dli "f" [("x", intT)] [] (Var () dli "x") intT
                    ))
                    `shouldBe`
                    Right (DefFun () dli "Mod1.f" [("Mod1.-fun_f.x", intT)] [] (Var () dli "Mod1.-fun_f.x") intT)
            it "keeps variables from its body in scope for the return expression" do
                testQual [] (qualifyStatement (
                    DefFun () dli "f" [] [Decl () dli "y" Nothing (IntLit () dli 5)] (Var () dli "y") intT
                    ))
                    `shouldBe`
                    Right (DefFun () dli "Mod1.f" [] [Decl () dli "Mod1.-fun_f.y" Nothing (IntLit () dli 5)] (Var () dli "Mod1.-fun_f.y") intT)
            it "is added to the names in the current Scope" do
                testQual [] (traverse qualifyStatement [
                      DefFun() dli "f" [] [CallFun () dli (Var () dli "f") []] (FCall () dli (Var () dli "f") []) intT
                    , CallFun () dli (Var () dli "f") []
                    ])
                    `shouldBe`
                    Right [
                      DefFun () dli "Mod1.f" [] [CallFun () dli (Var () dli "Mod1.f") []] (FCall () dli (Var () dli "Mod1.f") []) intT
                    , CallFun () dli (Var () dli "Mod1.f") []
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
                      Decl () dli "x" (Just intT) (IntLit () dli 5)
                    , Assign () dli "x" (Var () dli "x")
                    ])
                    `shouldBe`
                    Right [
                      Decl () dli "Mod1.x" (Just intT) (IntLit () dli 5)
                    , Assign () dli "Mod1.x" (Var () dli "Mod1.x")
                    ]
        --TODO
        describe "While" do
            it "creates a new scope for its body" do
                testQual [] (traverse qualifyStatement [
                      Decl () dli "x" (Just intT) (IntLit () dli 5)
                    , While () dli (BoolLit () dli True) [
                        Decl () dli "x" (Just intT) (IntLit () dli 6)
                      , Assign () dli "x" (Var () dli "x")
                      ]
                    , Assign () dli "x" (Var () dli "x")
                    ])
                    `shouldBe`
                    Right [
                      Decl () dli "Mod1.x" (Just intT) (IntLit () dli 5)
                    , While () dli (BoolLit () dli True) [
                        Decl () dli "Mod1.-while1.x" (Just intT) (IntLit () dli 6)
                      , Assign () dli "Mod1.-while1.x" (Var () dli "Mod1.-while1.x")
                      ]
                    , Assign () dli "Mod1.x" (Var () dli "Mod1.x")
                    ] -}
        describe "DefStruct" do
            it "correctly qualifies its field types" do
                testQual [] (qualifyStatement (DefStruct () dli "SomeStruct" [("x", intT)]))
                    `shouldBe`
                    Right (DefStruct () dli "Mod1.SomeStruct" [("Mod1.SomeStruct.x", intT)])
            it "is available inside its field types (structural recursion)" do -- TODO: Direct structural recursion might have to be detected
                testQual [] (qualifyStatement (DefStruct () dli "SomeStruct" [("f", TCon "SomeStruct" ())]))
                    `shouldBe`
                    Right (DefStruct () dli "Mod1.SomeStruct" [("Mod1.SomeStruct.f", TCon "Mod1.SomeStruct" KStar)])
            {-it "is added to the type names in the current scopes" do
                testQual [] (traverse qualifyStatement [
                      DefStruct () dli "SomeStruct" []
                    , Decl () dli "x" (Just (TCon "SomeStruct" ())) (IntLit () dli 42)
                    ])
                    `shouldBe`
                    Right [
                      DefStruct () dli "Mod1.SomeStruct" []
                    , Decl () dli "Mod1.x" (Just (TCon "Mod1.SomeStruct" KStar)) (IntLit () dli 42)
                    ]
            it "does not bleed into the outer scope" do
                testQual [] (traverse qualifyStatement [
                      DefFun () dli "f" [] [
                        DefStruct () dli "SomeStruct" []
                      ] (UnitLit dli) unitT
                      , Decl () dli "x" (Just (TCon "SomeStruct" ())) (IntLit () dli 42)
                    ])
                    `shouldBe`
                    Left (TypeNotFound dli "SomeStruct") -}
    {-describe "qualifyExpr" do
        describe "FCall" do
            it "finds the function name from the outer scope" do
                testQual [Scope "Mod2" [] ["f"] mempty] (qualifyExp (
                        FCall () dli (Var () dli "f") []
                    ))
                    `shouldBe`
                    Right (FCall () dli (Var () dli "Mod2.f") [])
            it "fully qualifies its arguments" do
                testQual [Scope "Mod2" [] ["f", "x"] mempty] (qualifyExp (
                    FCall () dli (Var () dli "f") [(Var () dli "x"), (Var () dli "f")]
                    ))
                    `shouldBe`
                    Right (FCall () dli (Var () dli "Mod2.f") [Var () dli "Mod2.x", Var () dli "Mod2.f"])  -}
        describe "IntLit" do
            it "is completely unaffected" do
                testQual [] (qualifyExp (IntLit () dli 42))
                    `shouldBe`
                    Right (IntLit () dli 42)
        {-describe "BoolLit" do
            it "is completely unaffected" do
                testQual [] (qualifyExp (BoolLit () dli True))
                    `shouldBe`
                    Right (BoolLit () dli True) -}
        describe "Var" do
            it "finds its name in the surrounding environment" do
                testQual [Scope "Mod2" [] ["x"] mempty] (qualifyExp (Var () dli "x"))
                   `shouldBe`
                   Right (Var () dli "Mod2.x")
            it "throws an error if the name does not exist" do
                testQual [] (qualifyExp (Var () dli "x"))
                    `shouldBe`
                    Left (NameNotFound dli "x")

    describe "qualifyMod" do
        it "does *not* prefix the mod name" do
            testQual [Scope "Mod" [] [] mempty] (qualifyMod (Module mempty "MyMod" []))
                `shouldBe`
                Right (Module mempty "MyMod" [])
        {-it "prefixes all statements" do
            testQual [Scope "Mod" [] [] mempty] (qualifyMod (Module mempty "MyMod" [Decl () dli "x" Nothing (IntLit () dli 5)]))
                `shouldBe`
                Right (Module mempty "MyMod" [Decl () dli "Mod.x" Nothing (IntLit () dli 5)]) -}
    describe "qualifyType" do
        pass

--TODO

dli :: LexInfo
dli = LexInfo (SourcePos 0 0) (SourcePos 0 0) "Test"

testQual :: [Scope]
         -> Sem '[State Int, State [Scope], Error QualificationError, Output Log, Reader Dependencies] a
         -> Either QualificationError a
testQual = testQual' mempty

testQual' :: Dependencies 
         -> [Scope]
         -> Sem '[State Int, State [Scope], Error QualificationError, Output Log, Reader Dependencies] a
         -> Either QualificationError a
testQual' deps s = run . runReader deps . runOutputSem (const pass) . runError . evalState (s ++ [Scope "Mod1" [] [] mempty, Scope "prims" ["Int", "Bool", "Unit"] [] (fromList [("Int", KStar), ("Bool", KStar), ("Unit", KStar)])]) . evalState 0

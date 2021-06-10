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
                    CallFun IgnoreExt dli (Var IgnoreExt dli "f") []
                    ))
                    `shouldBe`
                    Right (CallFun IgnoreExt dli (Var IgnoreExt dli "Mod2.f") [])
            it "does not use a type name" do
                testQual [Scope "Mod2" ["f"] [] mempty] (qualifyStatement (
                    CallFun IgnoreExt dli (Var IgnoreExt dli "f") []
                    ))
                    `shouldBe`
                    Left (NameNotFound dli "f")
            it "throws an error if the type is not found" do
                testQual [Scope "Mod2" [] ["g"] mempty] (qualifyStatement (
                    CallFun IgnoreExt dli (Var IgnoreExt dli "f") []
                    ))
                    `shouldBe`
                    Left (NameNotFound dli "f")
            it "does not start a new qualification context for its parameters" do
                testQual [Scope "Mod2" [] ["f", "x"] mempty, Scope "Mod1" [] ["y"] mempty] (qualifyStatement (
                    CallFun IgnoreExt dli (Var IgnoreExt dli "f") [Var IgnoreExt dli "x", Var IgnoreExt dli "y"]
                    ))
                    `shouldBe`
                    Right (CallFun IgnoreExt dli (Var IgnoreExt dli "Mod2.f") [Var IgnoreExt dli "Mod2.x", Var IgnoreExt dli "Mod1.y"])
        describe "DefVoid" do
            it "carries over the previous qualification context" do
                testQual [] (qualifyStatement (
                    DefFun IgnoreExt dli "f" [("x", intT)] [] (UnitLit dli) unitT
                    ))
                    `shouldBe`
                    Right (DefFun IgnoreExt dli "Mod1.f" [("Mod1.-fun_f.x", intT)] [] (UnitLit dli) unitT)
            it "starts a new qualification context in its body" do
                testQual [] (qualifyStatement (
                    DefFun IgnoreExt dli "f" [] [Decl IgnoreExt dli "x" Nothing (IntLit IgnoreExt dli 5)] (UnitLit dli) unitT
                    ))
                    `shouldBe`
                    Right (DefFun IgnoreExt dli "Mod1.f" [] [Decl IgnoreExt dli "Mod1.-fun_f.x" Nothing (IntLit IgnoreExt dli 5)] (UnitLit dli) unitT)
            it "is added to the names in the current Scope" do
                testQual [] (traverse qualifyStatement [
                      DefFun IgnoreExt dli "f" [] [CallFun IgnoreExt dli (Var IgnoreExt dli "f") []] (UnitLit dli) unitT
                    , CallFun IgnoreExt dli (Var IgnoreExt dli "f") []
                    ])
                    `shouldBe`
                    Right [
                      DefFun IgnoreExt dli "Mod1.f" [] [CallFun IgnoreExt dli (Var IgnoreExt dli "Mod1.f") []] (UnitLit dli) unitT
                    , CallFun IgnoreExt dli (Var IgnoreExt dli "Mod1.f") []
                    ]
            it "keeps its parameters in scope for the body" do
                testQual [] (qualifyStatement (
                    DefFun IgnoreExt dli "f" [("x", intT)] [Decl IgnoreExt dli "y" Nothing (Var IgnoreExt dli "x")] (UnitLit dli) unitT
                    ))
                    `shouldBe`
                    Right (DefFun IgnoreExt dli "Mod1.f" [("Mod1.-fun_f.x", intT)] [Decl IgnoreExt dli "Mod1.-fun_f.y" Nothing (Var IgnoreExt dli "Mod1.-fun_f.x")] (UnitLit dli) unitT)
        describe "DefFun" do
            it "carries over the previous qualification context" do
                testQual [] (qualifyStatement (
                    DefFun IgnoreExt dli "f" [("x", intT)] [] (IntLit IgnoreExt dli 5) intT
                    ))
                    `shouldBe`
                    Right (DefFun IgnoreExt dli "Mod1.f" [("Mod1.-fun_f.x", intT)] [] (IntLit IgnoreExt dli 5) intT)
            it "starts a new qualification context in its body" do
                testQual [] (qualifyStatement (
                    DefFun IgnoreExt dli "f" [] [Decl IgnoreExt dli "x" Nothing (IntLit IgnoreExt dli 5)] (IntLit IgnoreExt dli 5) intT
                    ))
                    `shouldBe`
                    Right (DefFun IgnoreExt dli "Mod1.f" [] [Decl IgnoreExt dli "Mod1.-fun_f.x" Nothing (IntLit IgnoreExt dli 5)] (IntLit IgnoreExt dli 5) intT)
            it "keeps its parameters in scope for the body and return expression" do
                testQual [] (qualifyStatement (
                    DefFun IgnoreExt dli "f" [("x", intT)] [Decl IgnoreExt dli "y" Nothing (Var IgnoreExt dli "x")] (Var IgnoreExt dli "x") intT
                    ))
                    `shouldBe`
                    Right (DefFun IgnoreExt dli "Mod1.f" [("Mod1.-fun_f.x", intT)] [Decl IgnoreExt dli "Mod1.-fun_f.y" Nothing (Var IgnoreExt dli "Mod1.-fun_f.x")] (Var IgnoreExt dli "Mod1.-fun_f.x") intT)
            it "starts a new qualification context in its return expression" do
                testQual [] (qualifyStatement (
                    DefFun IgnoreExt dli "f" [("x", intT)] [] (Var IgnoreExt dli "x") intT
                    ))
                    `shouldBe`
                    Right (DefFun IgnoreExt dli "Mod1.f" [("Mod1.-fun_f.x", intT)] [] (Var IgnoreExt dli "Mod1.-fun_f.x") intT)
            it "keeps variables from its body in scope for the return expression" do
                testQual [] (qualifyStatement (
                    DefFun IgnoreExt dli "f" [] [Decl IgnoreExt dli "y" Nothing (IntLit IgnoreExt dli 5)] (Var IgnoreExt dli "y") intT
                    ))
                    `shouldBe`
                    Right (DefFun IgnoreExt dli "Mod1.f" [] [Decl IgnoreExt dli "Mod1.-fun_f.y" Nothing (IntLit IgnoreExt dli 5)] (Var IgnoreExt dli "Mod1.-fun_f.y") intT)
            it "is added to the names in the current Scope" do
                testQual [] (traverse qualifyStatement [
                      DefFunIgnoreExt dli "f" [] [CallFun IgnoreExt dli (Var IgnoreExt dli "f") []] (FCall IgnoreExt dli (Var IgnoreExt dli "f") []) intT
                    , CallFun IgnoreExt dli (Var IgnoreExt dli "f") []
                    ])
                    `shouldBe`
                    Right [
                      DefFun IgnoreExt dli "Mod1.f" [] [CallFun IgnoreExt dli (Var IgnoreExt dli "Mod1.f") []] (FCall IgnoreExt dli (Var IgnoreExt dli "Mod1.f") []) intT
                    , CallFun IgnoreExt dli (Var IgnoreExt dli "Mod1.f") []
                    ]
        describe "Decl" do
            it "is added to the names in the current scope" do
                testQual [] (traverse qualifyStatement [
                      Decl IgnoreExt dli "x" Nothing (IntLit IgnoreExt dli 5)
                    , Decl IgnoreExt dli "y" Nothing (Var IgnoreExt dli "x")
                    ])
                    `shouldBe`
                    Right [
                      Decl IgnoreExt dli "Mod1.x" Nothing (IntLit IgnoreExt dli 5)
                    , Decl IgnoreExt dli "Mod1.y" Nothing (Var IgnoreExt dli "Mod1.x")
                    ]
        describe "Assign" do
            it "finds the variable from the surrounding environment" do
                testQual [] (traverse qualifyStatement [
                      Decl IgnoreExt dli "x" (Just intT) (IntLit IgnoreExt dli 5)
                    , Assign IgnoreExt dli "x" (Var IgnoreExt dli "x")
                    ])
                    `shouldBe`
                    Right [
                      Decl IgnoreExt dli "Mod1.x" (Just intT) (IntLit IgnoreExt dli 5)
                    , Assign IgnoreExt dli "Mod1.x" (Var IgnoreExt dli "Mod1.x")
                    ]
        --TODO
        describe "While" do
            it "creates a new scope for its body" do
                testQual [] (traverse qualifyStatement [
                      Decl IgnoreExt dli "x" (Just intT) (IntLit IgnoreExt dli 5)
                    , While IgnoreExt dli (BoolLit IgnoreExt dli True) [
                        Decl IgnoreExt dli "x" (Just intT) (IntLit IgnoreExt dli 6)
                      , Assign IgnoreExt dli "x" (Var IgnoreExt dli "x")
                      ]
                    , Assign IgnoreExt dli "x" (Var IgnoreExt dli "x")
                    ])
                    `shouldBe`
                    Right [
                      Decl IgnoreExt dli "Mod1.x" (Just intT) (IntLit IgnoreExt dli 5)
                    , While IgnoreExt dli (BoolLit IgnoreExt dli True) [
                        Decl IgnoreExt dli "Mod1.-while1.x" (Just intT) (IntLit IgnoreExt dli 6)
                      , Assign IgnoreExt dli "Mod1.-while1.x" (Var IgnoreExt dli "Mod1.-while1.x")
                      ]
                    , Assign IgnoreExt dli "Mod1.x" (Var IgnoreExt dli "Mod1.x")
                    ] -}
        describe "DefStruct" do
            it "correctly qualifies its field types" do
                testQual [] (qualifyStatement (DefStruct IgnoreExt dli "SomeStruct" [("x", intT)]))
                    `shouldBe`
                    Right (DefStruct IgnoreExt dli "Mod1.SomeStruct" [("Mod1.SomeStruct.x", intT)])
            it "is available inside its field types (structural recursion)" do -- TODO: Direct structural recursion might have to be detected
                testQual [] (qualifyStatement (DefStruct IgnoreExt dli "SomeStruct" [("f", TCon "SomeStruct" ())]))
                    `shouldBe`
                    Right (DefStruct IgnoreExt dli "Mod1.SomeStruct" [("Mod1.SomeStruct.f", TCon "Mod1.SomeStruct" KStar)])
            {-it "is added to the type names in the current scopes" do
                testQual [] (traverse qualifyStatement [
                      DefStruct IgnoreExt dli "SomeStruct" []
                    , Decl IgnoreExt dli "x" (Just (TCon "SomeStruct" IgnoreExt)) (IntLit IgnoreExt dli 42)
                    ])
                    `shouldBe`
                    Right [
                      DefStruct IgnoreExt dli "Mod1.SomeStruct" []
                    , Decl IgnoreExt dli "Mod1.x" (Just (TCon "Mod1.SomeStruct" KStar)) (IntLit IgnoreExt dli 42)
                    ]
            it "does not bleed into the outer scope" do
                testQual [] (traverse qualifyStatement [
                      DefFun IgnoreExt dli "f" [] [
                        DefStruct IgnoreExt dli "SomeStruct" []
                      ] (UnitLit dli) unitT
                      , Decl IgnoreExt dli "x" (Just (TCon "SomeStruct" IgnoreExt)) (IntLit IgnoreExt dli 42)
                    ])
                    `shouldBe`
                    Left (TypeNotFound dli "SomeStruct") -}
    {-describe "qualifyExpr" do
        describe "FCall" do
            it "finds the function name from the outer scope" do
                testQual [Scope "Mod2" [] ["f"] mempty] (qualifyExp (
                        FCall IgnoreExt dli (Var IgnoreExt dli "f") []
                    ))
                    `shouldBe`
                    Right (FCall IgnoreExt dli (Var IgnoreExt dli "Mod2.f") [])
            it "fully qualifies its arguments" do
                testQual [Scope "Mod2" [] ["f", "x"] mempty] (qualifyExp (
                    FCall IgnoreExt dli (Var IgnoreExt dli "f") [(Var IgnoreExt dli "x"), (Var IgnoreExt dli "f")]
                    ))
                    `shouldBe`
                    Right (FCall IgnoreExt dli (Var IgnoreExt dli "Mod2.f") [Var IgnoreExt dli "Mod2.x", Var IgnoreExt dli "Mod2.f"])  -}
        describe "IntLit" do
            it "is completely unaffected" do
                testQual [] (qualifyExp (IntLit IgnoreExt dli 42))
                    `shouldBe`
                    Right (IntLit IgnoreExt dli 42)
        {-describe "BoolLit" do
            it "is completely unaffected" do
                testQual [] (qualifyExp (BoolLit IgnoreExt dli True))
                    `shouldBe`
                    Right (BoolLit IgnoreExt dli True) -}
        describe "Var" do
            it "finds its name in the surrounding environment" do
                testQual [Scope "Mod2" [] mempty ["x"] mempty] (qualifyExp (Var IgnoreExt dli "x"))
                   `shouldBe`
                   Right (Var IgnoreExt dli "Mod2.x")
            it "throws an error if the name does not exist" do
                testQual [] (qualifyExp (Var IgnoreExt dli "x"))
                    `shouldBe`
                    Left (NameNotFound dli "x")

    describe "qualifyMod" do
        it "does *not* prefix the mod name" do
            testQual [Scope "Mod" [] mempty [] mempty] (qualifyMod (Module (Ext mempty) "MyMod" []))
                `shouldBe`
                Right (Module (Ext mempty) "MyMod" [])
        {-it "prefixes all statements" do
            testQual [Scope "Mod" [] [] mempty] (qualifyMod (Module mempty "MyMod" [Decl IgnoreExt dli "x" Nothing (IntLit IgnoreExt dli 5)]))
                `shouldBe`
                Right (Module mempty "MyMod" [Decl IgnoreExt dli "Mod.x" Nothing (IntLit IgnoreExt dli 5)]) -}
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
testQual' deps s = run . runReader deps . runOutputSem (const pass) . runError . evalState (s ++ [Scope "Mod1" [] mempty [] mempty, Scope "prims" ["Int", "Bool", "Unit"] mempty [] (fromList [("Int", KStar), ("Bool", KStar), ("Unit", KStar)])]) . evalState 0

module Language.Cobble.ParserSpec where

import Language.Cobble.Prelude.Parser hiding (assign)

import Language.Cobble.Types
import Language.Cobble.Parser
import Language.Cobble.Parser.Tokenizer

import qualified Data.Text as T

import Test.Hspec

spec :: Spec
spec = do
    describe "statement" pass
        {-describe "callFun" pass
        -- TODO
        describe "defVoid" do
            it "parses correct inputs without parameters and an empty body" do
                testParse defVoid "void procIgnoreExt{}"
                    `shouldBe`
                    Right (DefFun IgnoreExt (LexInfo 1 1 "Test") "proc" [] [] (UnitLit (LexInfo 1 1 "Test")) unitT)
            it "parses correctly with inputs" do
                testParse defVoid "void proc(x : Int, y: Bool){}"
                    `shouldBe`
                    Right (DefFun IgnoreExt (LexInfo 1 1 "Test") "proc" [("x", intT), ("y", boolT)] [] (UnitLit (LexInfo 1 1 "Test")) unitT)
            it "parses correctly with a body" do
                testParse defVoid (T.unlines [
                      "void procIgnoreExt"
                    , "{"
                    , "    let x = 5;"
                    , "    x = 6;"
                    , "}"
                    ])
                    `shouldBe`
                    Right (DefFun IgnoreExt (LexInfo 1 1 "Test") "proc" [] [
                          Decl IgnoreExt (LexInfo 3 5 "Test") "x" Nothing (IntLit IgnoreExt (LexInfo 3 13 "Test") 5)
                        , Assign IgnoreExt (LexInfo 4 5 "Test") "x" (IntLit IgnoreExt (LexInfo 4 9 "Test") 6)
                        ] (UnitLit (LexInfo 1 1 "Test")) unitT)
            it "can be chosen by statement" do
                testParse statement "void procIgnoreExt{}"
                    `shouldBe`
                    Right (DefFun IgnoreExt (LexInfo 1 1 "Test") "proc" [] [] (UnitLit (LexInfo 1 1 "Test")) unitT)
        describe "defFun" do
            it "parses correct inputs without params and an empty body (with braces)" do
                testParse defFun "Int fIgnoreExt{} => 5"
                    `shouldBe`
                    Right (DefFun IgnoreExt (LexInfo 1 1 "Test") "f" [] [] (IntLit IgnoreExt (LexInfo 1 14 "Test") 5) intT)
            it "parses correct inputs without params and an empty body (without braces)" do
                testParse defFun "Int fIgnoreExt => 5"
                    `shouldBe`
                    Right (DefFun IgnoreExt (LexInfo 1 1 "Test") "f" [] [] (IntLit IgnoreExt (LexInfo 1 12 "Test") 5) intT)
            it "parses correct inputs with params" do
                testParse defFun "Int idInt(x : Int, y: Bool) => x"
                    `shouldBe`
                    Right (DefFun IgnoreExt (LexInfo 1 1 "Test") "idInt" [("x", intT), ("y", boolT)] [] (Var IgnoreExt (LexInfo 1 32 "Test") "x") intT)
            it "parses correct inputs with a body" do
                testParse defFun (T.unlines [
                      "Int someFunction(x: Int)"
                    , "{"
                    , "    let y = x;"
                    , "} => f(y)"
                    ])
                    `shouldBe`
                    Right (DefFun IgnoreExt (LexInfo 1 1 "Test") "someFunction" [("x", intT)]
                        [ Decl IgnoreExt (LexInfo 3 5 "Test") "y" Nothing (Var IgnoreExt (LexInfo 3 13 "Test") "x")]
                        (FCall IgnoreExt (LexInfo 4 6 "Test") (Var IgnoreExt (LexInfo 4 6 "Test") "f") [Var IgnoreExt (LexInfo 4 8 "Test") "y"]) intT)
            it "can be chosen by statement" do
                testParse statement (T.unlines [
                      "Int someFunction(x: Int)"
                    , "{"
                    , "    let y = x;"
                    , "} => f(y)"
                    ])
                    `shouldBe`
                    Right (DefFun IgnoreExt (LexInfo 1 1 "Test") "someFunction" [("x", intT)]
                        [ Decl IgnoreExt (LexInfo 3 5 "Test") "y" Nothing (Var IgnoreExt (LexInfo 3 13 "Test") "x")]
                        (FCall IgnoreExt (LexInfo 4 6 "Test") (Var IgnoreExt (LexInfo 4 6 "Test") "f") [Var IgnoreExt (LexInfo 4 8 "Test") "y"]) intT)
        describe "decl" do
            it "parses correct inputs without a type sig" do
                testParse decl "let x = 5"
                    `shouldBe`
                    Right (Decl IgnoreExt (LexInfo 1 1 "Test") "x" Nothing (IntLit IgnoreExt (LexInfo 1 9 "Test") 5))
            it "parses correct inputs with a type sig" do
                testParse decl "let x : Int = 5"
                    `shouldBe`
                    Right (Decl IgnoreExt (LexInfo 1 1 "Test") "x" (Just intT) (IntLit IgnoreExt (LexInfo 1 15 "Test") 5))
            it "can be chosen by statement" do
                testParse statement "let x = 5"
                    `shouldBe`
                    Right (Decl IgnoreExt (LexInfo 1 1 "Test") "x" Nothing (IntLit IgnoreExt (LexInfo 1 9 "Test") 5))
                testParse statement "let x : Int = 5"
                    `shouldBe`
                    Right (Decl IgnoreExt (LexInfo 1 1 "Test") "x" (Just intT) (IntLit IgnoreExt (LexInfo 1 15 "Test") 5))
        describe "assign" do
            it "parses correct inputs" do
                testParse assign "x = 5"
                    `shouldBe`
                    Right (Assign IgnoreExt (LexInfo 1 1 "Test") "x" (IntLit IgnoreExt (LexInfo 1 5 "Test") 5))
            it "can be chosen by statement" do
                testParse statement "x = 5"
                    `shouldBe`
                    Right (Assign IgnoreExt (LexInfo 1 1 "Test") "x" (IntLit IgnoreExt (LexInfo 1 5 "Test") 5))

        describe "while" do
            it "parses correct inputs with an empty body" do
                testParse while "while(True){}"
                    `shouldBe`
                    Right (While IgnoreExt (LexInfo 1 1 "Test") (BoolLit IgnoreExt (LexInfo 1 7 "Test") True) [])
            it "parses correct inputs with a body" do
                testParse while (T.unlines [
                        "while(True)"
                      , "{"
                      , "    let x = 5;"
                      , "    x = 6;"
                      , "}"
                      ])
                    `shouldBe`
                    Right (While IgnoreExt (LexInfo 1 1 "Test") (BoolLit IgnoreExt (LexInfo 1 7 "Test") True) [
                          Decl IgnoreExt (LexInfo 3 5 "Test") "x" Nothing (IntLit IgnoreExt (LexInfo 3 13 "Test") 5)
                        , Assign IgnoreExt (LexInfo 4 5 "Test") "x" (IntLit IgnoreExt (LexInfo 4 9 "Test") 6)
                        ])
            it "can be chosen by statement" do
                testParse statement (T.unlines [
                        "while(True)"
                      , "{"
                      , "    let x = 5;"
                      , "    x = 6;"
                      , "}"
                      ])
                    `shouldBe`
                    Right (While IgnoreExt (LexInfo 1 1 "Test") (BoolLit IgnoreExt (LexInfo 1 7 "Test") True) [
                          Decl IgnoreExt (LexInfo 3 5 "Test") "x" Nothing (IntLit IgnoreExt (LexInfo 3 13 "Test") 5)
                        , Assign IgnoreExt (LexInfo 4 5 "Test") "x" (IntLit IgnoreExt (LexInfo 4 9 "Test") 6)
                        ])-}
    describe "expr" do
        {-describe "fcall" do
            it "parses correct inputs without parameters" do
                testParse fcall "fIgnoreExt"
                    `shouldBe`
                    Right (FCall IgnoreExt (LexInfo 1 1 "Test") (Var IgnoreExt (LexInfo 1 1 "Test") "f") [])
            it "parses correct inputs with parameters" do
                testParse fcall "f(1, True)"
                    `shouldBe`
                    Right (FCall IgnoreExt (LexInfo 1 1 "Test") (Var IgnoreExt (LexInfo 1 1 "Test") "f") [IntLit IgnoreExt (LexInfo 1 3 "Test") 1
                        , BoolLit IgnoreExt (LexInfo 1 6 "Test") True])
            it "correctly handles nested function calls" do
                testParse fcall "f(1, g(2))"
                    `shouldBe`
                    Right (FCall IgnoreExt (LexInfo 1 1 "Test") (Var IgnoreExt (LexInfo 1 1 "Test") "f") [IntLit IgnoreExt (LexInfo 1 3 "Test") 1
                            , FCall IgnoreExt (LexInfo 1 6 "Test") (Var IgnoreExt (LexInfo 1 6 "Test") "g") [IntLit IgnoreExt (LexInfo 1 8 "Test") 2]])
                            -}
            {-it "can be chosen by expr" do
                testParse expr "fIgnoreExt"
                    `shouldBe`
                    Right (FCall IgnoreExt (LexInfo 1 1 "Test") (Var IgnoreExt (LexInfo 1 1 "Test") "f") [])
                testParse expr "f(1, True)"
                    `shouldBe`
                    Right (FCall IgnoreExt (LexInfo 1 1 "Test") (Var IgnoreExt (LexInfo 1 1 "Test") "f") [IntLit IgnoreExt (LexInfo 1 3 "Test") 1
                        , BoolLit IgnoreExt (LexInfo 1 6 "Test") True])-}
        describe "intLit" do
            it "parses correct inputs" do
                testParse intLit "5432"
                    `shouldBe`
                    Right (LexInfo (SourcePos 1 1) (SourcePos 1 5) "Test", 5432)
            it "can be chosen by expr" do
                testParse expr "54321"
                    `shouldBe`
                    Right (IntLit IgnoreExt (LexInfo (SourcePos 1 1) (SourcePos 1 6) "Test") 54321)
        {-describe "boolLit" do
            it "parses correct inputs" do
                testParse boollit "True" `shouldBe` Right (BoolLit IgnoreExt (LexInfo 1 1 "Test") True)
                testParse boollit "False" `shouldBe` Right (BoolLit IgnoreExt (LexInfo 1 1 "Test") False)
            it "can be chosen by expr" do
                testParse expr "True" `shouldBe` Right (BoolLit IgnoreExt (LexInfo 1 1 "Test") True)
                testParse expr "False" `shouldBe` Right (BoolLit IgnoreExt (LexInfo 1 1 "Test") False) -}
        describe "varOrConstr" do
            it "parses correct inputs" do
                testParse varOrConstr "x" `shouldBe` Right (Var IgnoreExt (LexInfo (SourcePos 1 1) (SourcePos 1 2) "Test") "x")
            it "can be chosen by expr" do
                testParse expr "x" `shouldBe` Right (Var IgnoreExt (LexInfo (SourcePos 1 1) (SourcePos 1 2) "Test") "x")

data TestError = ParseE ParseError
               | LexicalE LexicalError
               deriving (Show, Eq)

testParse :: Parser a -> Text -> Either TestError a
testParse p t = do
    ts <- first LexicalE $ run $ runError $ tokenize "Test" t
    first ParseE $ parse p "Test" ts



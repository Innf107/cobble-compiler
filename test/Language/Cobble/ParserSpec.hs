module Language.Cobble.ParserSpec where

import Language.Cobble.Prelude.Parser hiding (assign)

import Language.Cobble.Types
import Language.Cobble.Parser
import Language.Cobble.Parser.Tokenizer

import qualified Data.Text as T

import Test.Hspec

spec :: Spec
spec = do
    describe "statement" do
        describe "callFun" pass
        -- TODO
        describe "defVoid" do
            it "parses correct inputs without parameters and an empty body" do
                testParse defVoid "void proc(){}"
                    `shouldBe`
                    Right (DefVoid () (LexInfo 1 1 "Test") "proc" [] [])
            it "parses correctly with inputs" do
                testParse defVoid "void proc(x : int, y: bool){}"
                    `shouldBe`
                    Right (DefVoid () (LexInfo 1 1 "Test") "proc" [("x", IntT), ("y", BoolT)] [])
            it "parses correctly with a body" do
                testParse defVoid (T.unlines [
                      "void proc()"
                    , "{"
                    , "    let x = 5;"
                    , "    x = 6;"
                    , "}"
                    ])
                    `shouldBe`
                    Right (DefVoid () (LexInfo 1 1 "Test") "proc" [] [
                          Decl () (LexInfo 3 5 "Test") "x" Nothing (IntLit () (LexInfo 3 13 "Test") 5)
                        , Assign () (LexInfo 4 5 "Test") "x" (IntLit () (LexInfo 4 9 "Test") 6)
                        ])
            it "can be chosen by statement" do
                testParse statement "void proc(){}"
                    `shouldBe`
                    Right (DefVoid () (LexInfo 1 1 "Test") "proc" [] [])
        describe "defFun" do
            it "parses correct inputs without params and an empty body (with braces)" do
                testParse defFun "int f(){} => 5"
                    `shouldBe`
                    Right (DefFun () (LexInfo 1 1 "Test") "f" [] [] (IntLit () (LexInfo 1 14 "Test") 5) IntT)
            it "parses correct inputs without params and an empty body (without braces)" do
                testParse defFun "int f() => 5"
                    `shouldBe`
                    Right (DefFun () (LexInfo 1 1 "Test") "f" [] [] (IntLit () (LexInfo 1 12 "Test") 5) IntT)
            it "parses correct inputs with params" do
                testParse defFun "int idInt(x : int, y: bool) => x"
                    `shouldBe`
                    Right (DefFun () (LexInfo 1 1 "Test") "idInt" [("x", IntT), ("y", BoolT)] [] (Var () (LexInfo 1 32 "Test") "x") IntT)
            it "parses correct inputs with a body" do
                testParse defFun (T.unlines [
                      "int someFunction(x: int)"
                    , "{"
                    , "    let y = x;"
                    , "} => f(y)"
                    ])
                    `shouldBe`
                    Right (DefFun () (LexInfo 1 1 "Test") "someFunction" [("x", IntT)]
                        [ Decl () (LexInfo 3 5 "Test") "y" Nothing (Var () (LexInfo 3 13 "Test") "x")]
                        (FCall () (LexInfo 4 6 "Test") "f" [Var () (LexInfo 4 8 "Test") "y"]) IntT)
            it "can be chosen by statement" do
                testParse statement (T.unlines [
                      "int someFunction(x: int)"
                    , "{"
                    , "    let y = x;"
                    , "} => f(y)"
                    ])
                    `shouldBe`
                    Right (DefFun () (LexInfo 1 1 "Test") "someFunction" [("x", IntT)]
                        [ Decl () (LexInfo 3 5 "Test") "y" Nothing (Var () (LexInfo 3 13 "Test") "x")]
                        (FCall () (LexInfo 4 6 "Test") "f" [Var () (LexInfo 4 8 "Test") "y"]) IntT)
        describe "decl" do
            it "parses correct inputs without a type sig" do
                testParse decl "let x = 5"
                    `shouldBe`
                    Right (Decl () (LexInfo 1 1 "Test") "x" Nothing (IntLit () (LexInfo 1 9 "Test") 5))
            it "parses correct inputs with a type sig" do
                testParse decl "let x : int = 5"
                    `shouldBe`
                    Right (Decl () (LexInfo 1 1 "Test") "x" (Just IntT) (IntLit () (LexInfo 1 15 "Test") 5))
            it "can be chosen by statement" do
                testParse statement "let x = 5"
                    `shouldBe`
                    Right (Decl () (LexInfo 1 1 "Test") "x" Nothing (IntLit () (LexInfo 1 9 "Test") 5))
                testParse statement "let x : int = 5"
                    `shouldBe`
                    Right (Decl () (LexInfo 1 1 "Test") "x" (Just IntT) (IntLit () (LexInfo 1 15 "Test") 5))
        describe "assign" do
            it "parses correct inputs" do
                testParse assign "x = 5"
                    `shouldBe`
                    Right (Assign () (LexInfo 1 1 "Test") "x" (IntLit () (LexInfo 1 5 "Test") 5))
            it "can be chosen by statement" do
                testParse statement "x = 5"
                    `shouldBe`
                    Right (Assign () (LexInfo 1 1 "Test") "x" (IntLit () (LexInfo 1 5 "Test") 5))

        describe "while" do
            it "parses correct inputs with an empty body" do
                testParse while "while(True){}"
                    `shouldBe`
                    Right (While () (LexInfo 1 1 "Test") (BoolLit () (LexInfo 1 7 "Test") True) [])
            it "parses correct inputs with a body" do
                testParse while (T.unlines [
                        "while(True)"
                      , "{"
                      , "    let x = 5;"
                      , "    x = 6;"
                      , "}"
                      ])
                    `shouldBe`
                    Right (While () (LexInfo 1 1 "Test") (BoolLit () (LexInfo 1 7 "Test") True) [
                          Decl () (LexInfo 3 5 "Test") "x" Nothing (IntLit () (LexInfo 3 13 "Test") 5)
                        , Assign () (LexInfo 4 5 "Test") "x" (IntLit () (LexInfo 4 9 "Test") 6)
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
                    Right (While () (LexInfo 1 1 "Test") (BoolLit () (LexInfo 1 7 "Test") True) [
                          Decl () (LexInfo 3 5 "Test") "x" Nothing (IntLit () (LexInfo 3 13 "Test") 5)
                        , Assign () (LexInfo 4 5 "Test") "x" (IntLit () (LexInfo 4 9 "Test") 6)
                        ])
        describe "callStatementMacro" pass
        -- TODO
    describe "expr" do
        describe "fcall" do
            it "parses correct inputs without parameters" do
                testParse fcall "f()"
                    `shouldBe`
                    Right (FCall () (LexInfo 1 1 "Test") "f" [])
            it "parses correct inputs with parameters" do
                testParse fcall "f(1, True)"
                    `shouldBe`
                    Right (FCall () (LexInfo 1 1 "Test") "f" [IntLit () (LexInfo 1 3 "Test") 1
                        , BoolLit () (LexInfo 1 6 "Test") True])
            it "correctly handles nested function calls" do
                testParse fcall "f(1, g(2))"
                    `shouldBe`
                    Right (FCall () (LexInfo 1 1 "Test") "f" [IntLit () (LexInfo 1 3 "Test") 1
                            , FCall () (LexInfo 1 6 "Test") "g" [IntLit () (LexInfo 1 8 "Test") 2]])
            it "can be chosen by expr" do
                testParse expr "f()"
                    `shouldBe`
                    Right (FCall () (LexInfo 1 1 "Test") "f" [])
                testParse expr "f(1, True)"
                    `shouldBe`
                    Right (FCall () (LexInfo 1 1 "Test") "f" [IntLit () (LexInfo 1 3 "Test") 1
                        , BoolLit () (LexInfo 1 6 "Test") True])
        describe "intLit" do
            it "parses correct inputs" do
                testParse intLit "5432"
                    `shouldBe`
                    Right (LexInfo 1 1 "Test", 5432)
            it "can be chosen by expr" do
                testParse expr "54321"
                    `shouldBe`
                    Right (IntLit () (LexInfo 1 1 "Test") 54321)
        describe "boolLit" do
            it "parses correct inputs" do
                testParse boollit "True" `shouldBe` Right (BoolLit () (LexInfo 1 1 "Test") True)
                testParse boollit "False" `shouldBe` Right (BoolLit () (LexInfo 1 1 "Test") False)
            it "can be chosen by expr" do
                testParse expr "True" `shouldBe` Right (BoolLit () (LexInfo 1 1 "Test") True)
                testParse expr "False" `shouldBe` Right (BoolLit () (LexInfo 1 1 "Test") False)
        describe "var" do
            it "parses correct inputs" do
                testParse var "x" `shouldBe` Right (Var () (LexInfo 1 1 "Test") "x")
            it "can be chosen by expr" do
                testParse expr "x" `shouldBe` Right (Var () (LexInfo 1 1 "Test") "x")

data TestError = ParseE ParseError
               | LexicalE LexicalError
               deriving (Show, Eq)

testParse :: Parser a -> Text -> Either TestError a
testParse p t = do
    ts <- first LexicalE $ tokenize "Test" t
    first ParseE $ parse p "Test" ts



{-#LANGUAGE NoImplicitPrelude, BlockArguments, OverloadedStrings#-}
{-# LANGUAGE TypeApplications, DataKinds #-}
module Language.MCScript.Parser.PreprocessorSpec where

import Language.MCScript.Prelude
import Language.MCScript.Types
import Language.MCScript.Parser.Tokenizer
import Language.MCScript.Parser.Preprocessor

import Test.Hspec

-- "defmacro for ( x : statement ) ; defmacro foreach ( x : ident; y : expr )"

spec :: Spec
spec = do
    describe "findMacroNames" do
        it "finds all macro names" do
            testPreprocess findMacroNames [Reserved "defmacro", Ident "for", Paren "(", Ident "x", ReservedOp ":",
                    Ident "statement", Paren ")", ReservedOp ";", Reserved "defmacro", Ident "foreach", Paren "(", Ident "x",
                    ReservedOp ":", Ident "ident", ReservedOp ";", Ident "y", ReservedOp ":", Ident "expr", Paren ")"
                ]
                `shouldBe`
                Right ["for", "foreach"]
        it "throws if a `defmacro` keyword is followed by anything other than an identifier" do
            testPreprocess' findMacroNames [Token (LexInfo 1 1 "Test") (Reserved "defmacro"),
                Token (LexInfo 1 10 "Test") (Reserved "defmacro"), Token (LexInfo 1 19 "Test") (Ident "x")]
                `shouldBe`
                Left (MacroNameNotIdent (Token (LexInfo 1 10 "Test") (Reserved "defmacro")))
    describe "replaceMacroCalls" do
        it "replaces all used macro identifiers with MacroCall tokens" do
            map tokData (replaceMacroCalls (map dummyTok [Reserved "defmacro", Ident "for", Paren "(", Paren ")",
                ReservedOp ";", Ident "for", Paren "(", Paren ")", ReservedOp ";", Ident "foreach", ReservedOp "=",
                IntLiteral 5, ReservedOp ";", Ident "while", Paren "(", Paren ")", ReservedOp ";"
                ]) ["for", "foreach"])
                `shouldBe`
                [ Reserved "defmacro", MacroCall "for", Paren "(", Paren ")",
                  ReservedOp ";", MacroCall "for", Paren "(", Paren ")", ReservedOp ";", MacroCall "foreach", ReservedOp "=",
                  IntLiteral 5, ReservedOp ";", Ident "while", Paren "(", Paren ")", ReservedOp ";"]
    describe "preProcess" do
        it "replaces all defined macro call idents with MacroCall tokens" do
            map tokData <$> (preProcess (map dummyTok [Reserved "defmacro", Ident "for", Paren "(", Paren ")", ReservedOp ";",
                Ident "foreach", Paren "(", Paren ")", ReservedOp ";", Reserved "defmacro", Ident "foreach", Paren "(", Paren ")",
                Paren "{", Ident "for", Paren "(", Paren ")", Paren "}", ReservedOp ";", Ident "while"]))
                `shouldBe`
                Right [Reserved "defmacro", MacroCall "for", Paren "(", Paren ")", ReservedOp ";",
                   MacroCall "foreach", Paren "(", Paren ")", ReservedOp ";", Reserved "defmacro", MacroCall "foreach", Paren "(", Paren ")",
                   Paren "{", MacroCall "for", Paren "(", Paren ")", Paren "}", ReservedOp ";", Ident "while"]

dummyTok :: TokenData a -> Token a
dummyTok = Token (LexInfo 0 0 "Test")

testPreprocess' :: ([Token p] -> Sem '[Error PreprocessError] a) -> [Token p] -> Either PreprocessError a
testPreprocess' f = run . runError . f

testPreprocess :: ([Token p] -> Sem '[Error PreprocessError] a) -> [TokenData p] -> Either PreprocessError a
testPreprocess f = testPreprocess' f . map dummyTok
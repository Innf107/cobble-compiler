{-# LANGUAGE NoImplicitPrelude, BlockArguments, OverloadedStrings #-}
module Language.MCScript.Parser.TokenizerSpec where

import Language.MCScript.Prelude
import Language.MCScript.Parser.Tokenizer
import Language.MCScript.Types

import Test.Hspec

spec :: Spec
spec = do
    describe "tokenize" do
        it "accepts an empty input" do
            tokenize "Test" ""
                `shouldBe`
                Right []
        it "accepts an input consisting of only whitespace" do
            tokenize "Test" "    \n  \t \t \n\t \n"
                `shouldBe`
                Right []
        it "tokenizes seperate identifiers" do
            map tokData <$> tokenize "Test" "this is a test"
                `shouldBe`
                Right [Ident "this", Ident "is", Ident "a", Ident "test"]
        it "handles reserved identifiers" do
            map tokData <$> tokenize "Test" "this Falsee is True"
                `shouldBe`
                Right [Ident "this", Ident "Falsee", Ident "is", Reserved "True"]
        it "handles reserved operators" do
            map tokData <$> tokenize "Test" "x : y := 4;"
                `shouldBe`
                Right [Ident "x", ReservedOp ":", Ident "y", Operator ":=", IntLiteral 4, ReservedOp ";"]
        it "keeps lexical information" do
            tokenize "Test" "this is also\n True + //-/ (12 * 23 )"
                `shouldBe`
                Right [Token (LexInfo 1 1 "Test") (Ident "this"), Token (LexInfo 1 6 "Test") (Ident "is"),
                       Token (LexInfo 1 9 "Test") (Ident "also"), Token (LexInfo 2 2 "Test") (Reserved "True"),
                       Token (LexInfo 2 7 "Test") (Operator "+"), Token (LexInfo 2 9 "Test") (Operator "//-/"),
                       Token (LexInfo 2 14 "Test") (Paren "("), Token (LexInfo 2 15 "Test") (IntLiteral 12),
                       Token (LexInfo 2 18 "Test") (Operator "*"), Token (LexInfo 2 20 "Test") (IntLiteral 23), 
                       Token (LexInfo 2 23 "Test") (Paren ")")]
        it "rejects invalid characters" do
            tokenize "Test" "test `"
                `shouldBe`
                Left (LexicalError (LexInfo 1 6 "Test") (UnexpectedChar '`'))
            tokenize "Test" "test 1ab"
                `shouldBe`
                Left (LexicalError (LexInfo 1 7 "Test") (UnexpectedCharInIntLit 'a'))
            tokenize "Test" "ident'ifier"
                `shouldBe`
                Left (LexicalError (LexInfo 1 6 "Test") (UnexpectedCharInIdent '\''))
            tokenize "Test" "operator +-`+"
                `shouldBe`
                Left (LexicalError (LexInfo 1 12 "Test") (UnexpectedCharInOp '`'))
        it "does not need spaces between identifiers and operators" do
            map tokData <$> tokenize "Test" "a+b -c/ de\n*\nf\n"
                `shouldBe`
                Right [Ident "a", Operator "+", Ident "b", Operator "-", 
                    Ident "c", Operator "/", Ident "de", Operator "*", Ident "f"]
        it "does not need spaces between identifiers/operators and parentheses" do
            map tokData <$> tokenize "Test" "a[ b () ] +}- )de\n)"
                `shouldBe`
                Right [Ident "a", Paren "[", Ident "b", Paren "(", Paren ")", Paren "]", Operator "+", Paren "}",
                       Operator "-", Paren ")", Ident "de", Paren ")"]
          


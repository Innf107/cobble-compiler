{-# LANGUAGE NoImplicitPrelude, BlockArguments, OverloadedStrings #-}
module Language.Cobble.Parser.TokenizerSpec where

import Language.Cobble.Prelude
import Language.Cobble.Parser.Tokenizer 
import Language.Cobble.Types

import Test.Hspec

spec :: Spec
spec = do
    describe "tokenize" do
        it "accepts an empty input" do
            tokenizeTest ""
                `shouldBe`
                Right []
        it "accepts an input consisting of only whitespace" do
            tokenizeTest "    \n  \t \t \n\t \n"
                `shouldBe`
                Right []
        it "tokenizes seperate identifiers" do
            map tokData <$> tokenizeTest "this is a test"
                `shouldBe`
                Right [Ident "this", Ident "is", Ident "a", Ident "test"]
        it "handles reserved identifiers" do
            map tokData <$> tokenizeTest "this Falsee is a True import bool"
                `shouldBe`
                Right [Ident "this", Ident "Falsee", Ident "is", Ident "a", Ident "True", Reserved "import", Ident "bool"]
        it "handles reserved operators" do
            map tokData <$> tokenizeTest "x : y := 4;"
                `shouldBe`
                Right [Ident "x", ReservedOp ":", Ident "y", Operator ":=", IntLiteral 4, ReservedOp ";"]
        it "keeps lexical information" do
            tokenizeTest "this is also\n True + //-/ (12 * 23 )"
                `shouldBe`
                Right [Token (LexInfo 1 1 "Test") (Ident "this"), Token (LexInfo 1 6 "Test") (Ident "is"),
                       Token (LexInfo 1 9 "Test") (Ident "also"), Token (LexInfo 2 2 "Test") (Ident "True"),
                       Token (LexInfo 2 7 "Test") (Operator "+"), Token (LexInfo 2 9 "Test") (Operator "//-/"),
                       Token (LexInfo 2 14 "Test") (Paren "("), Token (LexInfo 2 15 "Test") (IntLiteral 12),
                       Token (LexInfo 2 18 "Test") (Operator "*"), Token (LexInfo 2 20 "Test") (IntLiteral 23), 
                       Token (LexInfo 2 23 "Test") (Paren ")")]
        it "rejects invalid characters" do
            tokenizeTest "test `"
                `shouldBe`
                Left (LexicalError (LexInfo 1 6 "Test") (UnexpectedChar '`' Default))
            tokenizeTest "test 1ab"
                `shouldBe`
                Left (LexicalError (LexInfo 1 7 "Test") (UnexpectedChar 'a' (InIntLit "1")))
            tokenizeTest "ident'ifier"
                `shouldBe`
                Left (LexicalError (LexInfo 1 6 "Test") (UnexpectedChar '\'' (InIdent "ident")))
            tokenizeTest "operator +-`+"
                `shouldBe`
                Left (LexicalError (LexInfo 1 12 "Test") (UnexpectedChar '`' (InOp "+-")))
        it "does not need spaces between identifiers and operators" do
            map tokData <$> tokenizeTest "a+b -c/ de\n*\nf\n"
                `shouldBe`
                Right [Ident "a", Operator "+", Ident "b", Operator "-", 
                    Ident "c", Operator "/", Ident "de", Operator "*", Ident "f"]
            map tokData <$> tokenizeTest "x : int, y: bool"
                `shouldBe`
                Right [Ident "x", ReservedOp ":", Ident "int", ReservedOp ",", Ident "y", ReservedOp ":", Ident "bool"]
        it "does not need spaces between identifiers/operators and parentheses" do
            map tokData <$> tokenizeTest "a[ b () ] +}- )de\n)"
                `shouldBe`
                Right [Ident "a", Paren "[", Ident "b", Paren "(", Paren ")", Paren "]", Operator "+", Paren "}",
                       Operator "-", Paren ")", Ident "de", Paren ")"]
          

tokenizeTest :: Text -> Either LexicalError [Token]
tokenizeTest = run . runError . tokenize "Test" 


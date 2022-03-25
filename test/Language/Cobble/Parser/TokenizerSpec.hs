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
                Right [Ident "this", Ident "is", Ident "a", Ident "test", Dedent]
        it "handles reserved identifiers" do
            map tokData <$> tokenizeTest "this Falsee is a True import bool"
                `shouldBe`
                Right [Ident "this", Ident "Falsee", Ident "is", Ident "a", Ident "True", Reserved "import", Ident "bool", Dedent]
        it "handles reserved operators" do
            map tokData <$> tokenizeTest "x : y := 4;"
                `shouldBe`
                Right [Ident "x", Operator ":", Ident "y", Operator ":=", IntLiteral 4, ReservedOp ";", Dedent]
        it "keeps lexical information" do
            tokenizeTest "this is also\n True + //-/ (12 * 23 )"
                `shouldBe`
                Right [Token (LexInfo (SourcePos 1 1) (SourcePos 1 5) "Test") (Ident "this"),
                       Token (LexInfo (SourcePos 1 6) (SourcePos 1 8) "Test") (Ident "is"),
                       Token (LexInfo (SourcePos 1 9) (SourcePos 1 13) "Test") (Ident "also"),
                       Token (LexInfo (SourcePos 2 2) (SourcePos 2 6) "Test") (Ident "True"),
                       Token (LexInfo (SourcePos 2 7) (SourcePos 2 8) "Test") (Operator "+"),
                       Token (LexInfo (SourcePos 2 9) (SourcePos 2 13) "Test") (Operator "//-/"),
                       Token (LexInfo (SourcePos 2 14) (SourcePos 2 15) "Test") (Paren "("),
                       Token (LexInfo (SourcePos 2 15) (SourcePos 2 17) "Test") (IntLiteral 12),
                       Token (LexInfo (SourcePos 2 18) (SourcePos 2 19) "Test") (Operator "*"),
                       Token (LexInfo (SourcePos 2 20) (SourcePos 2 22) "Test") (IntLiteral 23),
                       Token (LexInfo (SourcePos 2 23) (SourcePos 2 24) "Test") (Paren ")")]
        it "rejects invalid characters" do
            tokenizeTest "test `"
                `shouldBe`
                Left (LexicalError (SourcePos 1 6) "Test" (UnexpectedChar '`' Default))
            tokenizeTest "ident'ifier"
                `shouldBe`
                Left (LexicalError (SourcePos 1 6) "Test" (UnexpectedChar '\'' (InIdent "ident")))
            tokenizeTest "operator +-`+"
                `shouldBe`
                Left (LexicalError (SourcePos 1 12) "Test" (UnexpectedChar '`' (InOp "+-")))
        it "does not need spaces between identifiers and operators" do
            map tokData <$> tokenizeTest "a+b -c/ de\n*\nf\n"
                `shouldBe`
                Right [Ident "a", Operator "+", Ident "b", Operator "-", 
                    Ident "c", Operator "/", Ident "de", Dedent, Operator "*", Dedent, Ident "f", Dedent]
            map tokData <$> tokenizeTest "x : int, y: bool"
                `shouldBe`
                Right [Ident "x", Operator ":", Ident "int", ReservedOp ",", Ident "y", Operator ":", Ident "bool", Dedent]
        it "does not need spaces between identifiers/operators and parentheses" do
            map tokData <$> tokenizeTest "a[ b () ] +}- )de\n)"
                `shouldBe`
                Right [Ident "a", Paren "[", Ident "b", Paren "(", Paren ")", Paren "]", Operator "+", Paren "}",
                       Operator "-", Paren ")", Ident "de", Dedent, Paren ")", Dedent]
          
        it "inserts Dedent and BlockEnd" do
            map tokData <$> tokenizeTest (unlines [
                    "f :: Int -> Int"
                ,   "f x = g x"
                ,   "      h x"
                ]) `shouldBe`
                    Right [
                            Ident "f", ReservedOp "::", Ident "Int", ReservedOp "->", Ident "Int", Dedent
                        ,   Ident "f", Ident "x", ReservedOp "=", Ident "g", Ident "x", Dedent
                        ,                                         Ident "h", Ident "x", BlockEnd
                        ]


tokenizeTest :: Text -> Either LexicalError [Token]
tokenizeTest = run . runError . tokenize "Test"


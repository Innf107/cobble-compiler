{-# LANGUAGE NoOverloadedStrings, OverloadedLists #-}
module Cobble.Parser.Tokenizer where

import Cobble.Prelude hiding (op, (|>))

import Cobble.Types

import Data.Char
import Data.DList as D

import Text.Read (read)

(|>) :: DList a -> a -> DList a
(|>) = D.snoc

-- | A Token consists of its lexical information
-- as well as the actual token data.
data Token = Token {
      tokLexInfo::LexInfo
    , tokData :: TokenData
    }  deriving (Show, Eq)


data TokenData = Ident Text
         | Reserved Text
         | Paren Text
         | Operator Text
         | ReservedOp Text
         | IntLiteral Int
         deriving (Show, Eq)

isOpStart :: Char -> Bool
isOpStart c = c /= '-' && isOpLetter c

isOpLetter :: Char -> Bool
isOpLetter = flip elem "+-*/~^!?.|<>$&=:;,\\"

isIntStart :: Char -> Bool
isIntStart x = isDigit x

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c `elem` "_"

isIdentLetter :: Char -> Bool
isIdentLetter c = isAlphaNum c || c `elem` "_#"

reserved :: [String]
reserved = ["let", "in", "if", "then", "else", "module", "import", "struct", "variant", "case", "of", "class", "instance", "infixl", "infixr", "forall"]

reservedOps :: [String]
reservedOps = [".", "::", ";", ",", "=", "=>", "->", "|", "\\"]

isParen :: Char -> Bool
isParen = (`elem`"()[]{}")

isNumPrefix :: Char -> Bool
isNumPrefix c = c `elem` "+-"

data LexicalError = LexicalError SourcePos FileName LexicalErrorData deriving (Show, Eq)

data LexicalErrorData = ReachedEOF TokenState
                  | UnexpectedChar Char (TokenState)
                  deriving (Show, Eq)

data TokenState = Default
                | InIdent (DList Char)
                | InOp (DList Char)
                | InIntLit (DList Char)
                | LeadingMinus
                | InComment
                deriving (Show, Eq)

sameTag :: TokenState -> TokenState -> Bool
sameTag Default         Default         = True
sameTag (InIdent _)     (InIdent _)     = True
sameTag (InOp _)        (InOp _)        = True
sameTag (InIntLit _)    (InIntLit _ )   = True
sameTag LeadingMinus    LeadingMinus    = True
sameTag InComment       InComment       = True
sameTag _               _               = False

tokenize :: forall r. (Members '[Error LexicalError {-, Output Log-}] r) => FileName -> Text -> Sem r [Token]
tokenize fp content = D.toList <$> go (SourcePos 1 1) (SourcePos 1 1) Default (toString content)
    where
        go :: SourcePos -> SourcePos -> TokenState -> [Char] -> Sem r (DList Token)
        go startPos endPos tokState' cs' = {-output (Log LogDebugVeryVerbose (show (startPos, endPos, tokState', cs'))) >>-} case (tokState', cs') of
            (Default, []) -> pure []
            (Default, (c:_))
                | isWhiteSpace c -> go' Default
                | isIdentStart c -> go' (InIdent [c])
                | isOpStart c    -> go' (InOp [c])
                | isIntStart c   -> go' (InIntLit [c])
                | isParen c      -> Paren [c] ++> go' Default
                | c == '-'       -> go' LeadingMinus
            (InIdent ident, []) -> done (Ident (toText ident))
            (InIdent ident, (c:_))
                | isIdentLetter c -> go' (InIdent (ident |> c))
                | isWhiteSpace c  -> mkIdent ident +> go' Default
                | isOpStart c     -> mkIdent ident +> go' (InOp [c])
                | isParen c       -> mkIdent ident +> Paren (toText c) ++> go' Default
                | c == '-'        -> go' LeadingMinus
            (InOp op, []) -> done (mkOperator op)
            (InOp op, (c:_))
                | isOpLetter c   -> go' (InOp (op |> c))
                | isWhiteSpace c -> mkOperator op +> go' Default
                | isIdentStart c -> mkOperator op +> go' (InIdent [c])
                | isIntStart c   -> mkOperator op +> go' (InIntLit [c])
                | isParen c      -> mkOperator op +> Paren (toText c) ++> go' Default
            (InIntLit lit, []) -> done (mkIntLit lit)
            (InIntLit lit, (c:_))
                | isDigit c      -> go' (InIntLit (lit |> c))
                | isWhiteSpace c -> mkIntLit lit +> go' Default
                | isIdentStart c -> mkIntLit lit +> go' (InIdent [c])
                | isOpStart c    -> mkIntLit lit +> go' (InOp [c])
                | isParen c      -> mkIntLit lit +> Paren (toText c) ++> go' Default
                | c == '-'       -> mkIntLit lit +> go' LeadingMinus
            (LeadingMinus, []) -> done (mkOperator ['-'])
            (LeadingMinus, (c:_))
                | c == '-'       -> go' InComment
                | isOpLetter c   -> go' (InOp ['-', c])
                | isWhiteSpace c -> mkOperator ['-'] +> go' Default
                | isIntStart c   -> go' (InIntLit ['-', c])
                | isParen c      -> mkOperator ['-'] +> (Paren [c] ++> go' Default)
                | isIdentStart c -> mkOperator ['-'] +> go' (InIdent [c])
            (InComment, []) -> pure []
            (InComment, (c:_))
                | isNewline c   -> go' Default
                | otherwise     -> go' InComment
            (ts, (c:_)) -> throw' (UnexpectedChar c ts)
            where
                go' :: TokenState -> Sem r (DList Token)
                go' ts = do
                    (endPos', cs) <- case cs' of
                        []        -> throw' (ReachedEOF ts)
                        ('\n':cs) -> pure (SourcePos{line=line endPos + 1, column=1}, cs)
                        (_:cs)    -> pure (endPos{column=column endPos + 1}, cs)
                    go (if sameTag ts tokState' then startPos else endPos) endPos' ts cs

                (+>) :: TokenData -> Sem r (DList Token) -> Sem r (DList Token)
                td +> rest = D.cons (Token (LexInfo startPos endPos fp) td) <$> rest
                (++>) :: TokenData -> Sem r (DList Token) -> Sem r (DList Token)
                td ++> rest = D.cons (Token (LexInfo endPos endPos{column=column endPos + 1} fp) td) <$> rest
                infixr 5 +>
                done :: TokenData -> Sem r (DList Token)
                done = pure . pure . Token (LexInfo startPos endPos fp)
                throw' :: LexicalErrorData -> Sem r a
                throw' = throw . LexicalError endPos fp
                mkIdent :: DList Char -> TokenData
                mkIdent (D.toList -> idStr)
                    | idStr `elem` reserved = Reserved (toText idStr)
                    | otherwise = Ident (toText idStr)
                mkOperator :: DList Char -> TokenData
                mkOperator (D.toList -> opStr)
                    | opStr `elem` reservedOps = ReservedOp (toText opStr)
                    | otherwise = Operator (toText opStr)
                mkIntLit :: DList Char -> TokenData
                mkIntLit dl = IntLiteral $ read $ D.toList dl


isNewline :: Char -> Bool
isNewline = (=='\n')

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

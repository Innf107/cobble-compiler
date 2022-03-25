{-# LANGUAGE NoOverloadedStrings, OverloadedLists #-}
module Language.Cobble.Parser.Tokenizer where

import Language.Cobble.Prelude hiding ((|>))

import Language.Cobble.Types

import Data.Char
import Data.DList (DList)
import Data.DList qualified as D

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
         | Dedent
         | BlockEnd
         deriving (Show, Eq)

isOpStart :: Char -> Bool
isOpStart c = c /= '-' && isOpLetter c

isOpLetter :: Char -> Bool
isOpLetter = flip elem "+-*/~^!?.|<>$&=#:;,\\"

isIntStart :: Char -> Bool
isIntStart x = isDigit x

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c `elem` "_"

isIdentLetter :: Char -> Bool
isIdentLetter c = isAlphaNum c || c `elem` "_"

reserved :: Set String
reserved = fromList ["let", "in", "if", "then", "else", "module", "import", "struct", "variant", "case", "of", "class", "instance", "infixl", "infixr", "forall", "do"]

reservedOps :: Set String
reservedOps = fromList [".", "::", ";", ",", "=", "=>", "->", "|", "\\"]

canStartBlock :: Set String
canStartBlock = fromList ["do", "of", "then", "else", "="]

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

tokenize :: forall r. (Members '[Error LexicalError] r) => FileName -> Text -> Sem r [Token]
tokenize fp content = D.toList <$> go (SourcePos 1 1) (SourcePos 1 1) [0] Default (toString content)
    where
        go :: SourcePos -> SourcePos -> [Int] -> TokenState -> [Char] -> Sem r (DList Token)
        go startPos endPos activeBlocks tokState' cs' = case (tokState', cs') of
            (Default, []) -> pure []
            (Default, (c:_))
                | isWhiteSpace c -> go' Default activeBlocks
                | isIdentStart c -> go' (InIdent [c]) activeBlocks
                | isOpStart c    -> go' (InOp [c]) activeBlocks
                | isIntStart c   -> go' (InIntLit [c]) activeBlocks
                | isParen c      -> Paren [c] ++> go' Default activeBlocks
                | c == '-'       -> go' LeadingMinus activeBlocks
            (InIdent ident, []) -> done (Ident (toText ident))
            (InIdent ident, (c:_))
                | isIdentLetter c -> go' (InIdent (ident |> c)) activeBlocks
                | isWhiteSpace c  -> mkIdent ident +|> go' Default
                | isOpStart c     -> mkIdent ident +|> go' (InOp [c]) 
                | isParen c       -> mkIdent ident +|> \blocks -> Paren (toText c) ++> go' Default blocks
                | c == '-'        -> go' LeadingMinus activeBlocks
            (InOp op, []) -> done (mkOperator op)
            (InOp op, (c:_))
                | isOpLetter c   -> go' (InOp (op |> c)) activeBlocks
                | isWhiteSpace c -> mkOperator op +> go' Default activeBlocks
                | isIdentStart c -> mkOperator op +> go' (InIdent [c]) activeBlocks
                | isIntStart c   -> mkOperator op +> go' (InIntLit [c]) activeBlocks
                | isParen c      -> mkOperator op +> Paren (toText c) ++> go' Default activeBlocks
            (InIntLit lit, []) -> done (mkIntLit lit)
            (InIntLit lit, (c:_))
                | isDigit c      -> go' (InIntLit (lit |> c)) activeBlocks
                | isWhiteSpace c -> mkIntLit lit +> go' Default activeBlocks
                | isIdentStart c -> mkIntLit lit +> go' (InIdent [c]) activeBlocks
                | isOpStart c    -> mkIntLit lit +> go' (InOp [c]) activeBlocks
                | isParen c      -> mkIntLit lit +> Paren (toText c) ++> go' Default activeBlocks
                | c == '-'       -> mkIntLit lit +> go' LeadingMinus activeBlocks
            (LeadingMinus, []) -> done (mkOperator ['-'])
            (LeadingMinus, (c:_))
                | c == '-'       -> go' InComment activeBlocks
                | isOpLetter c   -> go' (InOp ['-', c]) activeBlocks
                | isWhiteSpace c -> mkOperator ['-'] +> go' Default activeBlocks
                | isIntStart c   -> go' (InIntLit ['-', c]) activeBlocks
                | isParen c      -> mkOperator ['-'] +> Paren [c] ++> go' Default activeBlocks
                | isIdentStart c -> mkOperator ['-'] +> go' (InIdent [c]) activeBlocks
            (InComment, []) -> pure []
            (InComment, (c:_))
                | isNewline c   -> go' Default activeBlocks
                | otherwise     -> go' InComment activeBlocks
            (ts, (c:_)) -> throw' (UnexpectedChar c ts)
            where
                go' :: TokenState -> [Int] -> Sem r (DList Token)
                go' ts activeBlocks = do
                    (endPos', cs) <- case cs' of
                        []        -> throw' (ReachedEOF ts)
                        ('\n':cs) -> pure (SourcePos{line=line endPos + 1, column=1}, cs)
                        (_:cs)    -> pure (endPos{column=column endPos + 1}, cs)
                    go (if sameTag ts tokState' then startPos else endPos) endPos' activeBlocks ts cs

                -- | Continue by adding td to the output stream
                (+>) :: TokenData -> Sem r (DList Token) -> Sem r (DList Token)
                td +> rest = D.cons (Token (LexInfo startPos endPos fp) td) <$> rest
                infixr 5 +>
                -- | Just like (+>), but increases the endPos column for the added token by one
                (++>) :: TokenData -> Sem r (DList Token) -> Sem r (DList Token)
                td ++> rest = D.cons (Token (LexInfo endPos endPos{column=column endPos + 1} fp) td) <$> rest
                infixr 5 ++>
                
                -- | Just like (+>), but has the ability to introduce a new block
                (+|>) :: (TokenData, Maybe Int) -> ([Int] -> Sem r (DList Token)) -> Sem r (DList Token)
                (td, Nothing)    +|> rest = td +> rest activeBlocks
                (td, Just block) +|> rest = td +> rest (block : activeBlocks)

                done :: TokenData -> Sem r (DList Token)
                done = pure . pure . Token (LexInfo startPos endPos fp)

                throw' :: LexicalErrorData -> Sem r a
                throw' = throw . LexicalError endPos fp

                mkIdent :: DList Char -> (TokenData, Maybe Int)
                mkIdent (D.toList -> idStr)
                    | idStr `member` reserved = (Reserved (toText idStr), whenAlt (idStr `member` canStartBlock) (column startPos))
                    | otherwise = (Ident (toText idStr), Nothing)

                mkOperator :: DList Char -> TokenData
                mkOperator (D.toList -> opStr)
                    | opStr `member` reservedOps = ReservedOp (toText opStr)
                    | otherwise = Operator (toText opStr)

                mkIntLit :: DList Char -> TokenData
                mkIntLit dl = IntLiteral $ read $ D.toList dl


isNewline :: Char -> Bool
isNewline = (=='\n')

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

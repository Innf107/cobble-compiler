{-# LANGUAGE NoOverloadedStrings, OverloadedLists #-}
module Language.Cobble.Parser.Tokenizer where

import Language.Cobble.Prelude hiding (op)

import Language.Cobble.Types

import Data.Char
import Data.DList as D

import Text.Read (read)

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
isOpLetter = flip elem "+-*/~^!?.|<>$&=#:;,"

isIntStart :: Char -> Bool
isIntStart x = isDigit x

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c `elem` "_"

isIdentLetter :: Char -> Bool
isIdentLetter c = isAlphaNum c || c `elem` "_."

reserved :: [String]
reserved = ["let", "if", "then", "else", "import", "struct"]

reservedOps :: [String]
reservedOps = [":", "::", ";", ",", "=", "=>", "->"]

isParen :: Char -> Bool
isParen = (`elem`"()[]{}")

isNumPrefix :: Char -> Bool
isNumPrefix c = c `elem` "+-"

data LexicalError = LexicalError LexInfo LexicalErrorData deriving (Show, Eq)

data LexicalErrorData = ReachedEOF TokenState
                  | UnexpectedChar Char (TokenState)
                  deriving (Show, Eq)


type TokenizeC r = Members [State LexInfo, State (TokenState, LexInfo), Error LexicalError] r

askChar :: (TokenizeC r, Member (State [Char]) r) => Sem r (Maybe Char)
askChar = do
    get >>= \case
        [] -> pure Nothing
        (c:cs) -> do
            put cs
            if (c `isNewline`) then
                modify (\ls -> ls{line=line ls + 1, column=0})
            else
                modify (\ts -> ts{column=column ts + 1})
            pure $ Just c

--peekChar :: (TokenizeC r, Member (State [Char]) r) => Sem r (Maybe Char)
--peekChar = viaNonEmpty head <$> get

putS :: (TokenizeC r) => TokenState -> Sem r ()
putS ts = do
    li <- gets snd
    put (ts, li)

putStart :: (TokenizeC r) => TokenState -> Sem r ()
putStart ts = do
    li <- get
    put (ts, li)

throwL :: (TokenizeC r) => LexicalErrorData -> Sem r ()
throwL d = get >>= \li -> throw (LexicalError li d)

tellToken :: (TokenizeC r, Member (Writer [Token]) r) => TokenData -> Sem r ()
tellToken td = do
    lexInfo <- gets snd
    tell [Token lexInfo td]

tellTokenNewLex:: (TokenizeC r, Member (Writer [Token]) r) => TokenData -> Sem r ()
tellTokenNewLex td = do
    lexInfo <- get
    tell [Token lexInfo td]


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

type SourcePos = (Int, Int)

tokenize :: forall r. (Members '[Error LexicalError] r) => FileName -> Text -> Sem r [Token]
tokenize fp content = D.toList <$> go (1, 0) (1, 0) Default (toString content)
    where
        go :: SourcePos -> SourcePos -> TokenState -> [Char] -> Sem r (DList Token)
        go startPos endPos tokState' cs' = case (tokState', cs') of
            (Default, []) -> pure []
            (Default, (c:_))
                | isWhiteSpace c -> go' Default
                | isIdentStart c -> go' (InIdent [c])
                | isOpStart c    -> go' (InOp [c])
                | isIntStart c   -> go' (InIntLit [c])
                | isParen c      -> Paren (toText c) +> go' Default
                | c == '-'       -> go' LeadingMinus
            (InIdent ident, []) -> done (Ident (toText ident))
            (InIdent ident, (c:_))
                | isIdentLetter c -> go' (InIdent (ident |> c))
                | isWhiteSpace c  -> Ident (toText ident) +> go' Default
                | isOpStart c     -> Ident (toText ident) +> go' (InOp [c])
                | isParen c       -> Ident (toText ident) +> Paren (toText c) +> go' Default
                | c == '-'        -> go' LeadingMinus
            (InOp op, []) -> done (mkOperator op)
            (InOp op, (c:_))
                | isOpLetter c   -> go' (InOp (op |> c))
                | isWhiteSpace c -> mkOperator op +> go' Default
                | isIdentStart c -> mkOperator op +> go' (InIdent [c])
                | isIntStart c   -> mkOperator op +> go' (InIntLit [c])
                | isParen c      -> mkOperator op +> Paren (toText c) +> go' Default
            (InIntLit lit, []) -> done (mkIntLit lit)
            (InIntLit lit, (c:_))
                | isDigit c      -> go' (InIntLit (lit |> c))
                | isWhiteSpace c -> mkIntLit lit +> go' Default
                | isIdentStart c -> mkIntLit lit +> go' (InIdent [c])
                | isParen c      -> mkIntLit lit +> Paren (toText c) +> go' Default
                | c == '-'       -> mkIntLit lit +> go' LeadingMinus
            (LeadingMinus, []) -> done (mkOperator ['-'])
            (LeadingMinus, (c:_))
                | c == '-'       -> go' InComment
                | isOpLetter c   -> go' (InOp ['-', c])
                | isWhiteSpace c -> mkOperator ['-'] +> go' Default
                | isIntStart c   -> go' (InIntLit ['-', c])
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
                        ('\n':cs) -> pure ((fst endPos + 1, 1), cs)
                        (_:cs)    -> pure ((fst endPos, snd endPos + 1), cs)
                    go (if sameTag ts tokState' then startPos else endPos') endPos' ts cs

                (+>) :: TokenData -> Sem r (DList Token) -> Sem r (DList Token)
                td +> rest = D.cons (Token (LexInfo (fst startPos) (snd startPos) fp) td) <$> rest
                infixr 5 +>
                done :: TokenData -> Sem r (DList Token)
                done = pure . pure . Token (LexInfo (fst startPos) (snd startPos) fp)
                throw' :: LexicalErrorData -> Sem r a
                throw' = throw . LexicalError (LexInfo (fst startPos) (snd startPos) fp)
                mkOperator :: DList Char -> TokenData
                mkOperator (D.toList -> opStr)
                    | opStr `elem` reservedOps = ReservedOp (toText opStr)
                    | otherwise = Operator (toText opStr)
                mkIntLit :: DList Char -> TokenData
                mkIntLit dl = IntLiteral $ read $ D.toList dl

{-
tokenize' :: TokenizeC r => [Char] -> Sem r [Token]
tokenize' input = fmap fst $ runWriterAssocR $ evalState input $ go
    where
        go :: (TokenizeC r, Members [Writer [Token], State [Char]] r) => Sem r ()
        go = gets fst >>= \case
            Default -> askChar >>= \mc -> peekChar >>= \nc -> case mc of
                Nothing -> pass
                Just c -> if
                    | isIdentStart c -> putStart (InIdent [c]) >> go
                    | isNumStart c   -> putStart (InIntLit [c]) >> go
                    | isOpStart c    -> putStart (InOp [c]) >> go
                    | isWhiteSpace c -> go
                    | isParen c      -> tellTokenNewLex (Paren (one c)) >> putStart Default >> go
                    | c == '-'       -> putStart (InOp [c])
                    | otherwise -> throwL $ UnexpectedChar c
            InIdent cs -> askChar >>= \case
                Nothing -> endIdent cs
                Just c -> if
                    | isIdentLetter c -> putS (InIdent (cs <> [c])) >> go
                    | isWhiteSpace c -> endIdent (cs) >> putStart Default >> go
                    | isOpStart c -> endIdent cs >> putStart (InOp [c]) >> go
                    | isParen c -> endIdent cs >> tellTokenNewLex (Paren (one c)) >> putStart Default >> go
                    | otherwise -> throwL $ UnexpectedCharInIdent c
                -- TODO
                where
                    endIdent chars
                        | chars `elem` reserved = tellToken (Reserved (toText chars))
                        | otherwise = tellToken (Ident (toText chars))
            InOp cs -> askChar >>= \case
                Nothing -> endOp cs
                Just c -> if
                    | isOpLetter c -> putS (InOp (cs <> [c])) >> go
                    | isDigit c -> endOp cs >> putStart (InIntLit [c]) >> go
                    | isIdentStart c -> endOp cs >> putStart (InIdent [c]) >> go
                    | isWhiteSpace c -> endOp cs >> putStart Default >> go
                    | isParen c -> endOp cs >> tellTokenNewLex (Paren (one c)) >> putStart Default >> go
                    | otherwise -> throwL $ UnexpectedCharInOp c
                where
                    endOp chars
                        | chars `elem` reservedOps = tellToken (ReservedOp (toText chars))
                        | otherwise = tellToken (Operator (toText chars))
            InIntLit cs -> askChar >>= \case
                Nothing -> endIntLit cs
                Just c -> if
                    | isDigit c -> putS (InIntLit (cs <> [c])) >> go
                    | isWhiteSpace c -> endIntLit cs >> putS Default >> go
                    | isOpStart c -> endIntLit cs >> putS (InOp [c]) >> go
                    | isParen c -> endIntLit cs >> tellTokenNewLex (Paren (one c)) >> putStart Default >> go
                    | otherwise -> throwL $ UnexpectedCharInIntLit c
                where
                    endIntLit = tellToken . IntLiteral . read
-}
isNewline :: Char -> Bool
isNewline = (=='\n')

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

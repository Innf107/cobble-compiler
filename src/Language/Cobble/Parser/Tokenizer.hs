{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Parser.Tokenizer where

import Language.Cobble.Prelude hiding ((|>))

import Language.Cobble.Types

import Data.Char
import Data.DList (DList)
import Data.DList qualified as D

import Text.Read (read)

import Data.Text qualified as T

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
isOpLetter = flip elem ("+-*/~^!?.|<>$&=#:;,\\" :: String)

isIntStart :: Char -> Bool
isIntStart x = isDigit x

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c `elem` ("_" :: String)

isIdentLetter :: Char -> Bool
isIdentLetter c = isAlphaNum c || c `elem` ("_" :: String)

reserved :: Set Text
reserved = fromList ["let", "in", "if", "then", "else", "module", "import", "struct", "variant", "case", "of", "class", "where", "instance", "infixl", "infixr", "forall", "do"]

reservedOps :: Set Text
reservedOps = fromList [".", "::", ";", ",", "=", "=>", "->", "|", "\\"]

canStartBlock :: Set Text
canStartBlock = fromList ["do", "of", "then", "else", "=", "where"]

isParen :: Char -> Bool
isParen = (`elem`("()[]{}" :: String))

data LexicalError = LexicalError SourcePos FileName LexicalErrorData deriving (Show, Eq)

data LexicalErrorData = ReachedEOF TokenState
                      | UnexpectedChar Char (TokenState)
                      | ClosedNonexistentBlock
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

data LexState = LexState {
    _lexStartPos   :: SourcePos
,   _lexPos        :: SourcePos
,   _tokenState    :: TokenState
,   _activeBlocks  :: [SourcePos]
,   _startingBlock :: Bool
,   _skipDedent  :: Bool
}
makeLenses ''LexState

tokenize :: Members '[Error LexicalError] r => FileName -> Text -> Sem r [Token]
tokenize fileName content = fmap fst 
                          $ runOutputList 
                          $ evalState (LexState (SourcePos 1 1) (SourcePos 1 1) Default [] False True) 
                          $ go content
    where
        go :: Members '[State LexState, Error LexicalError, Output Token] r => Text -> Sem r ()
        go content = case T.uncons content of
            Just (c, rest) -> do
                when (not $ isWhiteSpace c) $ tryInsertDedent
                lex c
                increasePos c
                go rest
            -- EOF
            Nothing -> do
                lexEOF
                increasePos '\n'
                tryInsertDedent
        
        lex :: Members '[State LexState, Error LexicalError, Output Token] r => Char -> Sem r ()
        lex c = gets _tokenState >>= \case
            Default -> do
                modify(\s -> s & lexStartPos .~ (_lexPos s))
                if
                    | isWhiteSpace c    -> pure ()
                    | isIdentStart c    -> setState (InIdent [c])
                    | isOpStart c       -> setState (InOp [c])
                    | isIntStart c      -> setState (InIntLit [c])
                    | isParen c         -> outputParen c
                    | c == '-'          -> setState LeadingMinus
                    | otherwise         -> throwLex $ UnexpectedChar c Default
            InIdent ident
                | isWhiteSpace c    -> emitIdent ident >> setState Default
                | isIdentLetter c   -> setState (InIdent (ident |> c))
                | isOpStart c       -> emitIdent ident >> setState (InOp [c])
                | isParen c         -> emitIdent ident >> outputToken (Paren [c]) >> setState Default
                | c == '-'          -> emitIdent ident >> setState LeadingMinus
            InOp op
                | isWhiteSpace c    -> emitOp op >> setState Default
                | isOpLetter c      -> setState (InOp (op |> c))
                | isIdentStart c    -> emitOp op >> setState (InIdent [c])
                | isIntStart c      -> emitOp op >> setState (InIntLit [c])
                | isParen c         -> emitOp op >> outputToken (Paren [c]) >> setState Default
            InIntLit lit
                | isWhiteSpace c    -> emitIntLit lit >> setState Default
                | isDigit c         -> setState (InIntLit (lit |> c))
                | isIdentStart c    -> emitIntLit lit >> setState (InIdent [c])
                | isOpStart c       -> emitIntLit lit >> setState (InOp [c])
                | isParen c         -> emitIntLit lit >> outputToken (Paren [c]) >> setState Default
                | c == '-'          -> emitIntLit lit >> setState LeadingMinus
            LeadingMinus
                | isWhiteSpace c    -> emitOp ['-'] >> setState Default
                | isIdentLetter c   -> emitOp ['-'] >> setState (InIdent [c])
                | isOpLetter c      -> setState (InOp ['-', c])
                | isIntStart c      -> setState (InIntLit ['-', c])
                | isParen c         -> emitOp ['-'] >> outputToken (Paren [c]) >> setState Default
                | c == '-'          -> setState InComment
            InComment
                | isNewline c       -> setState Default
                | otherwise         -> pure ()
            s -> throwLex $ UnexpectedChar c s
           
        lexEOF :: Members '[State LexState, Error LexicalError, Output Token] r => Sem r ()
        lexEOF = gets _tokenState >>= \case
            Default         -> pure ()
            InIdent ident   -> emitIdent ident
            InOp op         -> emitOp op
            InIntLit lit    -> emitIntLit lit
            LeadingMinus    -> emitOp ['-']
            InComment       -> pure ()


        emitIdent :: Members '[State LexState, Output Token] r => DList Char -> Sem r ()
        emitIdent (toText -> ident)
            | ident `member` reserved = do
                outputToken (Reserved ident)
                when (ident `member` canStartBlock) $ introduceBlock
            | otherwise = outputToken (Ident ident)

        emitOp :: Members '[State LexState, Output Token] r => DList Char -> Sem r ()
        emitOp (toText -> op)
            | op `member` reservedOps = do
                outputToken (ReservedOp op)
                when (op `member` canStartBlock) $ introduceBlock
            | otherwise = outputToken (Operator op)


        emitIntLit :: Members '[State LexState, Output Token] r => DList Char -> Sem r ()
        emitIntLit = outputToken . IntLiteral . read . D.toList -- `read` is safe here, since any constructed int lit should be valid

        introduceBlock :: Members '[State LexState] r => Sem r ()
        introduceBlock = modify (startingBlock .~ True)

        increasePos :: Members '[State LexState, Error LexicalError] r => Char -> Sem r ()
        increasePos '\n' = modify (lexPos %~ \(SourcePos line column) -> SourcePos (line + 1) 1)
        increasePos _    = modify (lexPos %~ \(SourcePos line column) -> SourcePos line (column + 1))

        tryInsertDedent :: Members '[State LexState, Output Token, Error LexicalError] r => Sem r ()
        tryInsertDedent = do
            LexState{_lexPos=sourcePos, _startingBlock, _skipDedent} <- get
            when _startingBlock do
                modify (activeBlocks %~ (sourcePos :))
                modify (startingBlock .~ False)
            gets _activeBlocks >>= \case
                (block : remainingBlocks) -> do
                    when (line sourcePos > line block) do
                        when (column sourcePos == column block) $ outputToken Dedent
                        when (column sourcePos <  column block) do
                            outputToken BlockEnd
                            modify (skipDedent .~ True)
                            modify (activeBlocks .~ remainingBlocks)
                            tryInsertDedent -- Try again, since we might close multiple blocks at once.

                _ -> when (column sourcePos == 1) $ 
                        if not _skipDedent then 
                            outputToken Dedent
                        else
                            modify (skipDedent .~ False)

        -- Not super happy about this :/
        outputParen :: Members '[State LexState, Output Token] r => Char -> Sem r ()
        outputParen p = do
            LexState{_lexStartPos, _lexPos} <- get

            modify (lexStartPos .~ _lexPos)

            output (Token (LexInfo _lexStartPos (_lexPos{column = column _lexPos + 1}) fileName) (Paren [p]))

        outputToken :: Members '[State LexState, Output Token] r => TokenData -> Sem r ()
        outputToken tokData = do
            LexState{_lexStartPos, _lexPos} <- get

            modify (lexStartPos .~ _lexPos)

            output (Token (LexInfo _lexStartPos _lexPos fileName) tokData)

        setState :: Members '[State LexState] r => TokenState -> Sem r ()
        setState tokState = modify (tokenState .~ tokState)

        throwLex :: Members '[State LexState, Error LexicalError] r => LexicalErrorData -> Sem r a
        throwLex errData = do
            sourcePos <- gets _lexPos
            throw (LexicalError sourcePos fileName errData)


isNewline :: Char -> Bool
isNewline = (=='\n')

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` (" \t\n" :: String)

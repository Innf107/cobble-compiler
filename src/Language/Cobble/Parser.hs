module Language.Cobble.Parser where

import Language.Cobble.Prelude.Parser hiding (assign)
import Language.Cobble.Types
import Language.Cobble.Types.PrettyPrint
import Language.Cobble.Parser.Tokenizer (Token(..), TokenData(..))

import Data.Text qualified as T

import Data.Char

import Text.Parsec hiding ((<|>))
import Text.Parsec.Pos

type NextPass = 'SolveModules


type Parser = Parsec [Token] ()

(<??>) :: Text -> Parser a -> Parser a
t <??> p = p <?> toString t

infix 0 <??>

token' :: (Token -> Maybe a) -> Parser a
token' = token
    prettyPrintToken
    (\(Token LexInfo{line, column, file} _) -> newPos (toString file) line column)


ident :: Parser (LexInfo, Text)
ident = "identifier" <??> token' \case
    Token l (Ident t) -> Just (l, t)
    _ -> Nothing
    
ident' :: Parser Text
ident' = snd <$> ident
    
reserved :: Text -> Parser LexInfo
reserved r = (token' \case
    Token l (Reserved t) | r == t -> Just l
    _ -> Nothing) <?> toString r
    
paren :: Text -> Parser LexInfo
paren p = (token' \case
    Token l (Paren t) | t == p -> Just l
    _ -> Nothing) <?> toString p

paren' :: Text -> Parser ()
paren' = void . paren

operator :: Parser (LexInfo, Text)
operator = (token' \case
    Token l (Operator t) -> Just (l, t)
    _ -> Nothing) <?> "operator"

operator' :: Parser Text
operator' = snd <$> operator

reservedOp :: Text -> Parser LexInfo
reservedOp o = (token' \case
    Token l (ReservedOp t) | t == o -> Just l
    _ -> Nothing) <?> toString o
    
reservedOp' :: Text -> Parser ()
reservedOp' = void . reservedOp
    
intLit :: Parser (LexInfo, Int)
intLit = (token' \case
    Token l (IntLiteral i) -> Just (l, i)
    _ -> Nothing) <?> "integer literal"

module_ :: Text -> Parser (Module NextPass)
module_ mname = "module" <??> Module () mname <$> statements

statement :: Parser (Statement NextPass)
statement = "statement" <??> def -- <|> importS

expr :: Parser (Expr NextPass)
expr = "expr" <??> fcall <|> expr'

expr' :: Parser (Expr NextPass)
expr' = "expr (no fcall)" <??> uncurry (IntLit ()) <$> intLit <|> boollit <|> ifE <|> var <|> withParen expr




def :: Parser (Statement NextPass)
def = "function definition" <??> do
    ((li, t), fname) <- try $ (,) <$> typeP <*> ident' <* paren' "("
    ps <- map (\(_x, y, z) -> (y, z)) <$> typedIdent `sepBy` (reservedOp ",")
    paren' ")"
    reservedOp' "=>"
    ret <- expr
    pure $ Def () li fname ps  ret t


defStruct :: Parser (Statement NextPass)
defStruct = "struct definition" <??> DefStruct ()
    <$> reserved "struct"
    <*> ident'
    <*> between (paren' "{") (paren' "}") (typedIdent' `sepBy` (reservedOp' ","))


fcall :: Parser (Expr NextPass)
fcall = "function call" <??> do
    (li, fname) <- (\x -> (getLexInfo x, x)) <$> try (expr' <* paren' "(")
    ps <- expr `sepBy` reservedOp' ","
    paren' ")"
    pure $ FCall () li fname ps
   
   
boollit :: Parser (Expr NextPass)
boollit = "boolean literal" <??> choice [ reserved "True" >>= \li -> pure $ BoolLit () li True
                 , reserved "False" >>= \li -> pure $ BoolLit () li False
                 ]

ifE :: Parser (Expr NextPass)
ifE = "if expression" <??> If ()
    <$> reserved "if" <*> expr
    <*> (reserved "then" *> expr)
    <*> (reserved "else" *> expr)

var :: Parser (Expr NextPass)
var = "variable" <??> uncurry (Var ()) <$> ident

statementBody :: Parser [Statement NextPass]
statementBody = paren' "{" *> statements <* paren "}"

statements :: Parser [Statement NextPass]
statements = many (statement <* reservedOp ";")

typedIdent :: Parser (LexInfo, Text, Type NextPass)
typedIdent = "typed identifier" <??> do
    (li, n) <- ident 
    reservedOp' ":"
    (_, t) <- typeP
    pure (li, n, t)

typedIdent' :: Parser (Text, Type NextPass)
typedIdent' = (\(_, y, z) -> (y, z)) <$> typedIdent

mTypedIdent :: Parser (LexInfo, Text, Maybe (Type NextPass))
mTypedIdent = "optionally typed identifier" <??> do
        (li, n) <- ident
        mt <- optionMaybe $ reservedOp' ":" >> snd <$> typeP
        pure (li, n, mt)

typeP :: Parser (LexInfo, Type NextPass)
typeP = "type" <??> do
    (li, i) <- ident
    pure $ (li,) $ if isLower (T.head $ T.takeWhileEnd (/='.') i)
        then TVar i ()
        else TCon i ()

withParen :: Parser a -> Parser a
withParen a = paren "(" *> a <* paren ")"

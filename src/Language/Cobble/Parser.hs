{-#LANGUAGE NoImplicitPrelude, DataKinds, NamedFieldPuns, LambdaCase, BlockArguments, OverloadedStrings#-}
module Language.Cobble.Parser where

import Language.Cobble.Prelude.Parser hiding (assign)
import Language.Cobble.Types
import Language.Cobble.Parser.Tokenizer (Token(..), TokenData(..), Processing(..))

import Text.Parsec hiding ((<|>))
import Text.Parsec.Pos

type NextPass = ExpandMacros


type Parser = Parsec [Token 'Processed] ()

(<??>) :: Text -> Parser a -> Parser a
t <??> p = p <?> toString t

infix 0 <??>

token' :: (Token 'Processed -> Maybe a) -> Parser a
token' = token
    (\(Token _ t) -> show t) -- TODO: Better pretty printing function
    (\(Token LexInfo{line, column, file} _) -> newPos (toString file) line column)


ident :: Parser (LexInfo, Text)
ident = (token' \case
    Token l (Ident t) -> Just (l, t)
    _ -> Nothing) <?> "identifier" 
    
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
    
macroCall :: Parser (LexInfo, Text)
macroCall = (token' \case
    Token l (MacroCall m) -> Just (l, m)
    _ -> Nothing) <?> "macro name"
 
 
statement :: Parser (Statement NextPass)
statement = "statement" <??> {-callFun <|>-} defVoid <|> try defFun <|> decl <|> assign <|> while {- <|> defStruct -} <|> callStatementMacro
--                              ^ TODO

expr :: Parser (Expr NextPass)
expr = "expr" <??> uncurry (IntLit ()) <$> intLit <|> boollit <|> try fcall <|> var


defVoid :: Parser (Statement NextPass)
defVoid = "void definition" <??> do
    li <- reserved "void"
    fname <- ident'
    paren' "("
    ps <- map (\(x, y, z) -> (y, z)) <$> typedIdent `sepBy` (reservedOp ",")
    paren' ")"
    b <- statementBody
    pure $ DefVoid () li fname ps b

defFun :: Parser (Statement NextPass)
defFun = "function definition" <??> do
    (li, t) <- typeP
    fname <- ident'
    paren' "("
    ps <- map (\(x, y, z) -> (y, z)) <$> typedIdent `sepBy` (reservedOp ",")
    paren' ")"
    b <- option [] statementBody
    reservedOp' "=>"
    ret <- expr
    pure $ DefFun () li fname ps b ret t

decl :: Parser (Statement NextPass)
decl = "variable declaration" <??> do
    li <- reserved "let"
    (_, vname, mtype) <- mTypedIdent
    reservedOp' "="
    e <- expr
    pure $ Decl () li vname mtype e

assign :: Parser (Statement NextPass)
assign = "variable assignment" <??> do
    (li, vname) <- ident
    reservedOp' "="
    e <- expr
    pure $ Assign () li vname e

while :: Parser (Statement NextPass)
while = "while statement" <??> do
    li <- reserved "while"
    paren' "("
    e <- expr
    paren' ")"
    b <- statementBody
    pure $ While () li e b

callStatementMacro :: Parser (Statement NextPass)
callStatementMacro = "statement macro call" <??> do
    (li, mname) <- macroCall
    fail "StatementMacroParams: TODO!"
    pure $ CallStatementMacro li mname undefined

fcall :: Parser (Expr NextPass)
fcall = "function call" <??> do
    (li, fname) <- ident
    paren' "("
    ps <- expr `sepBy` reservedOp' ","
    paren' ")"
    pure $ FCall () li fname ps
   
boollit :: Parser (Expr NextPass)
boollit = "boolean literal" <??> choice [ reserved "True" >>= \li -> pure $ BoolLit () li True
                 , reserved "False" >>= \li -> pure $ BoolLit () li False
                 ]

var :: Parser (Expr NextPass)
var = "variable" <??> uncurry (Var ()) <$> ident

statementBody :: Parser [Statement NextPass]
statementBody = paren' "{" *> statements <* paren "}"

statements :: Parser [Statement NextPass]
statements = many (statement <* reservedOp ";")

typedIdent :: Parser (LexInfo, Text, TypeInfo NextPass)
typedIdent = "typed identifier" <??> do
    (li, n) <- ident 
    reservedOp' ":"
    (_, t) <- typeP
    pure (li, n, t)

mTypedIdent :: Parser (LexInfo, Text, Maybe (TypeInfo NextPass))
mTypedIdent = "optionally typed identifier" <??> do
        (li, n) <- ident
        mt <- optionMaybe $ reservedOp' ":" >> snd <$> typeP
        pure (li, n, mt)

typeP :: Parser (LexInfo, TypeInfo NextPass)
typeP = "type" <??> ident 

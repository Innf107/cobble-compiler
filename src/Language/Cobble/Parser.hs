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

unitLit :: Parser LexInfo
unitLit = try (paren "(" <* paren' ")")

module_ :: Text -> Parser (Module NextPass)
module_ mname = "module" <??> Module () mname <$> statements

statement :: Parser (Statement NextPass)
statement = "statement" <??> def <|> import_

expr :: Parser (Expr NextPass)
expr = "expression" <??> fcallOrVar <|> expr'

expr' :: Parser (Expr NextPass)
expr' = "expression (no fcall)" <??> uncurry (IntLit ()) <$> intLit <|> UnitLit <$> unitLit <|> ifE <|> var <|> withParen expr


def :: Parser (Statement NextPass)
def = "definition" <??> do
    (li, sigName, ty) <- signature 
    reservedOp' ";"
    name <- ident'
    when (name /= sigName) $ fail "Function definition does not immediately follow its type signature"
    
    params <- many ident'

    reservedOp' "="
    e <- expr
    pure $ Def () li name params e ty

import_ :: Parser (Statement NextPass)
import_ = "import" <??> Import ()
    <$> reserved "import"
    <*> modName

modName :: Parser (Name NextPass)
modName = ident'

signature :: Parser (LexInfo, Name NextPass, Type NextPass)
signature = "type signature" <??> do
    (li, i) <- ident
    reservedOp' "::"
    (_, t) <- typeP
    pure (li, i, t)

signature' :: Parser (Name NextPass, Type NextPass)
signature' = fmap (\(_, n, t) -> (n, t)) signature

defStruct :: Parser (Statement NextPass)
defStruct = "struct definition" <??> DefStruct ()
    <$> reserved "struct"
    <*> ident'
    <*> between (paren' "{") (paren' "}") (typedIdent' `sepBy` (reservedOp' ","))


fcallOrVar :: Parser (Expr NextPass)
fcallOrVar = "function call" <??> do
    f <- expr'
    args <- many expr'
    case args of
        [] -> pure f
        (a:as)  -> pure $ FCall () (getLexInfo f) f (a :| as)
   

ifE :: Parser (Expr NextPass)
ifE = "if expression" <??> If ()
    <$> reserved "if" <*> expr
    <*> (reserved "then" *> expr)
    <*> (reserved "else" *> expr)

var :: Parser (Expr NextPass)
var = "variable" <??> uncurry (Var ()) <$> ident

statements :: Parser [Statement NextPass]
statements = many (statement <* reservedOp ";")

typedIdent :: Parser (LexInfo, Text, Type NextPass)
typedIdent = "typed identifier" <??> do
    (li, n) <- ident 
    reservedOp' "::"
    (_, t) <- typeP
    pure (li, n, t)

typedIdent' :: Parser (Text, Type NextPass)
typedIdent' = (\(_, y, z) -> (y, z)) <$> typedIdent

typeP :: Parser (LexInfo, Type NextPass)
typeP = "type" <??> do
    (li, t1) <- namedType
    functionType li t1 <|> pure (li, t1)

namedType :: Parser (LexInfo, Type NextPass)
namedType = do
    (li, i) <- ident
    pure $ (li,) $ if isLower (T.head $ T.takeWhileEnd (/='.') i)
        then TVar i ()
        else TCon i ()

functionType :: LexInfo -> Type NextPass -> Parser (LexInfo, Type NextPass)
functionType li tyA = do
    reservedOp' "->"
    (_, tyB) <- typeP
    pure (li, tyA -:> tyB)

withParen :: Parser a -> Parser a
withParen a = paren "(" *> a <* paren ")"

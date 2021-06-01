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
    (\(Token LexInfo{startPos=SourcePos {line, column}, file} _) -> newPos (toString file) line column)


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
unitLit = try (mergeLexInfo <$> paren "(" <*> paren ")")

letE :: Parser (Expr NextPass)
letE = "let binding" <??> (\ls n ps e b -> Let () (mergeLexInfo ls (getLexInfo e)) (Decl () n ps e) b)
    <$> reserved "let"
    <*> ident'
    <*> many ident'
    <*  reservedOp' "="
    <*> expr
    <*  reserved "in"
    <*> expr

module_ :: Text -> Parser (Module NextPass)
module_ mname = "module" <??> Module () mname <$> statements

statement :: Parser (Statement NextPass)
statement = "statement" <??> def <|> import_

expr :: Parser (Expr NextPass)
expr = "expression" <??> fcallOrVar <|> expr'

expr' :: Parser (Expr NextPass)
expr' = "expression (no fcall)" <??> uncurry (IntLit ()) <$> intLit <|> UnitLit <$> unitLit <|> letE <|> ifE <|> var <|> withParen expr


def :: Parser (Statement NextPass)
def = "definition" <??> do
    (liStart, sigName, ty) <- signature 
    reservedOp' ";"
    name <- ident'
    when (name /= sigName) $ fail "Function definition does not immediately follow its type signature"
    
    params <- many ident'

    reservedOp' "="
    e <- expr
    pure $ Def () (mergeLexInfo liStart (getLexInfo e)) (Decl () name params e) ty

import_ :: Parser (Statement NextPass)
import_ = "import" <??> do
    liStart <- reserved "import"
    (liEnd, name) <- modName
    pure (Import () (liStart `mergeLexInfo` liEnd) name)

modName :: Parser (LexInfo, Name NextPass)
modName = ident

signature :: Parser (LexInfo, Name NextPass, Type NextPass)
signature = "type signature" <??> do
    (liStart, i) <- ident
    reservedOp' "::"
    (liEnd, t) <- typeP
    pure (liStart `mergeLexInfo` liEnd, i, t)

signature' :: Parser (Name NextPass, Type NextPass)
signature' = fmap (\(_, n, t) -> (n, t)) signature

defStruct :: Parser (Statement NextPass)
defStruct = "struct definition" <??> (\ls n fs le -> DefStruct () (ls `mergeLexInfo` le) n fs)
    <$> reserved "struct"
    <*> ident'
    <* paren' "{" 
    <*> typedIdent' `sepBy` (reservedOp' ",")
    <*> paren "}"


fcallOrVar :: Parser (Expr NextPass)
fcallOrVar = "function call" <??> do
    f <- expr'
    args <- many expr'
    case args of
        []      -> pure f
        (a:as)  -> pure $ FCall () (getLexInfo f `mergeLexInfo` (getLexInfo (last (a :| as)))) f (a :| as)
   

ifE :: Parser (Expr NextPass)
ifE = "if expression" <??> (\liStart te ee -> If () (liStart `mergeLexInfo` (getLexInfo ee)) te ee)
    <$> reserved "if" <*> expr
    <*> (reserved "then" *> expr)
    <*> (reserved "else" *> expr)

var :: Parser (Expr NextPass)
var = "variable" <??> uncurry (Var ()) <$> ident

statements :: Parser [Statement NextPass]
statements = many (statement <* reservedOp ";")

typedIdent :: Parser (LexInfo, Text, Type NextPass)
typedIdent = "typed identifier" <??> (\(ls, n) (le, t) -> (ls `mergeLexInfo` le, n, t))
    <$> ident 
    <*  reservedOp' "::"
    <*> typeP

typedIdent' :: Parser (Text, Type NextPass)
typedIdent' = (\(_, y, z) -> (y, z)) <$> typedIdent

typeP :: Parser (LexInfo, Type NextPass)
typeP = "type" <??> do
    (ls, t1) <- namedType
    functionType ls t1 <|> pure (ls, t1)

namedType :: Parser (LexInfo, Type NextPass)
namedType = do
    (li, i) <- ident
    pure $ if isLower (T.head $ T.takeWhileEnd (/='.') i)
        then (li, TVar i ())
        else (li, TCon i ())

functionType :: LexInfo -> Type NextPass -> Parser (LexInfo, Type NextPass)
functionType li tyA = do
    reservedOp' "->"
    (le, tyB) <- typeP
    pure (li `mergeLexInfo` le, tyA -:> tyB)

withParen :: Parser a -> Parser a
withParen a = paren "(" *> a <* paren ")"

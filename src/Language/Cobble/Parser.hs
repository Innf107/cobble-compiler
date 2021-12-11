module Language.Cobble.Parser where

import Language.Cobble.Prelude.Parser hiding (assign)
import Language.Cobble.Types
import Language.Cobble.Types.PrettyPrint
import Language.Cobble.Parser.Tokenizer (Token(..), TokenData(..))

import Data.Text qualified as T

import Data.Char

import Text.Parsec hiding ((<|>))
import Text.Parsec.Pos

import Data.List qualified as L

type NextPass = 'SolveModules


type Parser = Parsec [Token] ()

(<??>) :: Text -> Parser a -> Parser a
t <??> p = p <?> toString t

infix 0 <??>

token' :: (Token -> Maybe a) -> Parser a
token' = token
    prettyPrintToken
    (\(Token LexInfo{startPos=SourcePos {line, column}, file} _) -> newPos (toString file) line column)


identNoOperator :: Parser (LexInfo, Text)
identNoOperator = "identifier" <??> token' \case
    Token l (Ident t) -> Just (l, t)
    _ -> Nothing

ident :: Parser (LexInfo, Text)
ident = identNoOperator 
     <|> try (paren "(" *> operator) <* paren ")"

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
letE = "let binding" <??> (\ls n ps e b -> Let IgnoreExt (mergeLexInfo ls (getLexInfo e)) (Decl IgnoreExt n (Ext ps) e) b)
    <$> reserved "let"
    <*> ident'
    <*> many ident'
    <*  reservedOp' "="
    <*> expr
    <*  reserved "in"
    <*> expr

module_ :: Text -> Parser (Module NextPass)
module_ mname = "module" <??> Module IgnoreExt mname <$> statements <* eof

statement :: Parser (Statement NextPass)
statement = "statement" <??> def <|> defStruct <|> defVariant <|> defClass <|> defInstance <|> import_

expr :: Parser (Expr NextPass)
expr = merge 
        <$> exprWithoutAscription 
        <*> optionMaybe ascription
    where
        ascription = "ascription" 
            <??>  
            reservedOp "::"
            *> typeP
        merge e (Just (le, ty)) = Ascription IgnoreExt (mergeLexInfo (getLexInfo e) le) e ty
        merge e Nothing = e

exprWithoutAscription :: Parser (Expr NextPass)
exprWithoutAscription = exprOrOp <&> \case
    OpLeaf e -> e
    opGroup  -> ExprX opGroup (leftLI opGroup `mergeLexInfo` rightLI opGroup)  
    where
        leftLI (OpLeaf e) = getLexInfo e
        leftLI (OpNode l _ _) = leftLI l
        rightLI (OpLeaf e) = getLexInfo  e
        rightLI (OpNode _ _ r) = rightLI r


exprOrOp :: Parser (OperatorGroup NextPass NoFixity)
exprOrOp = do
    l <- OpLeaf <$> exprWithoutOp
    mrest <- optionMaybe $ (,)
        <$> operator'
        <*> exprOrOp
    pure case mrest of
        Nothing     -> l
        Just (o, r) -> OpNode l (o, ()) r

exprWithoutOp :: Parser (Expr NextPass)
exprWithoutOp = "expression" <??> do
        f <- expr'
        args <- many expr'
        case args of
            []      -> pure f
            (a:as)  -> pure $ FCall IgnoreExt (getLexInfo f `mergeLexInfo` (getLexInfo (last (a :| as)))) f (a :| as)

expr' :: Parser (Expr NextPass)
expr' = "expression (no fcall)" <??> (\e mf -> maybe e (\(le, fname) -> StructAccess IgnoreExt (getLexInfo e `mergeLexInfo` le) e fname) mf)
    <$> expr''
    <*> optionMaybe (reservedOp' "." *> ident) 

expr'' :: Parser (Expr NextPass)
expr'' = "expression (no fcall / struct access)" <??> uncurry (IntLit IgnoreExt) <$> intLit <|> UnitLit <$> unitLit <|> letE <|> ifE <|> varOrConstr <|> withParen expr


def :: Parser (Statement NextPass)
def = "definition" <??> do
    mfixity <- optionMaybe fixity
    (liStartSig, sigName, ty) <- signature 
    reservedOp' ";"

    defDecl@(Decl _ name _ e) <- decl

    when (name /= sigName) $ fail "Function definition does not immediately follow its type signature"
    
    pure $ Def (Ext (snd <$> mfixity)) 
            (maybe liStartSig fst mfixity `mergeLexInfo` getLexInfo e) 
            defDecl ty

decl :: Parser (Decl NextPass)
decl = "declaration" <??> (\f xs e -> Decl IgnoreExt f (Ext xs) e)
    <$> ident'
    <*> many ident'
    <* reservedOp' "="
    <*> expr

import_ :: Parser (Statement NextPass)
import_ = "import" <??> do
    liStart <- reserved "import"
    (liEnd, name) <- modName
    pure (Import IgnoreExt (liStart `mergeLexInfo` liEnd) name)

modName :: Parser (LexInfo, Name NextPass)
modName = joinSegments <$> some validSegment
    where
        joinSegments = L.foldr1 (\(l1, t1) (l2, t2) -> (l1 `mergeLexInfo` l2, t1 <> t2))
        validSegment = "module name" <??> token' \case
            Token l (Ident t)           -> Just (l, t)
            Token l (Operator "/")      -> Just (l, "/")
            Token l (ReservedOp ".")    -> Just (l, ".")
            _ -> Nothing

fixity :: Parser (LexInfo, Fixity)
fixity = "fixity declaration" <??> (\(ls, f) (le, i) -> (ls `mergeLexInfo` le, f i))
    <$> (   ((,LeftFix)  <$> reserved "infixl")
        <|> ((,RightFix) <$> reserved "infixr")
        )
    <*> intLit

defStruct :: Parser (Statement NextPass)
defStruct = "struct definition" <??> (\ls n ps fs le -> DefStruct IgnoreExt (ls `mergeLexInfo` le) n (map (\x -> MkTVar x ()) ps) fs)
    <$> reserved "struct"
    <*> ident'
    <*> many ident'
    <* paren' "{" 
    <*> signature' `sepBy` (reservedOp' ",")
    <*> paren "}"
   
defVariant :: Parser (Statement NextPass)
defVariant = "variant definition" <??> (\ls n ps cs -> DefVariant IgnoreExt (ls `mergeLexInfo` snd (unsafeLast cs)) n (map (`MkTVar`()) ps) (map (\((n,ts),_) -> (n, ts, IgnoreExt)) cs))
    <$> reserved "variant"
    <*> ident'
    <*> many ident'
    <*  reservedOp' "="
    <*> (constr `sepBy1` reservedOp' "|")
    where
        constr = "variant constructor definition" <??> (\(ls, i) ts -> ((i, map snd ts), foldr (\x r -> fst x `mergeLexInfo` r) ls ts))
            <$> ident
            <*> many namedType

defClass :: Parser (Statement NextPass)
defClass = "class definition" <??> (\ls n ps cs le -> DefClass IgnoreExt (ls `mergeLexInfo` le) n (map (\v -> MkTVar v ()) ps) cs)
    <$> reserved "class"
    <*> ident'
    <*> many ident'
    <*  paren' "{"
    <*> many (signature' <* reservedOp' ";")
    <*> paren "}"

defInstance :: Parser (Statement NextPass)
defInstance = "instance definition" <??> (\ls cn (_, t) ds le -> DefInstance IgnoreExt (ls `mergeLexInfo` le) cn t ds)
    <$> reserved "instance"
    <*> ident'
    <*> typeP
    <*  paren' "{"
    <*> many (decl <* reservedOp' ";")
    <*> paren "}"

ifE :: Parser (Expr NextPass)
ifE = "if expression" <??> (\liStart te ee -> If IgnoreExt (liStart `mergeLexInfo` (getLexInfo ee)) te ee)
    <$> reserved "if" <*> expr
    <*> (reserved "then" *> expr)
    <*> (reserved "else" *> expr)

varOrConstr :: Parser (Expr NextPass)
varOrConstr = "variable or struct construction" <??> do
    v <- ident
    m <- option False (paren' "{" >> pure True)
    case m of
        False -> pure $ makeVarOrVariantConstr v 
        True -> structConstructRest v
    where
        structConstructRest :: (LexInfo, Text) -> Parser (Expr NextPass)
        structConstructRest (ls, n) = "struct construction" <??> (\fs le -> StructConstruct IgnoreExt (ls `mergeLexInfo` le) n fs)
            <$> fieldUpdate `sepBy` (reservedOp' ",")
            <*> paren  "}"
            where
                fieldUpdate = (,) <$> ident' <* reservedOp' "=" <*> expr

        makeVarOrVariantConstr :: (LexInfo, Text) -> Expr NextPass
        makeVarOrVariantConstr (li, name)
            | isUpper (T.head name) = VariantConstr IgnoreExt li name
            | otherwise             = Var IgnoreExt li name

statements :: Parser [Statement NextPass]
statements = many (statement <* reservedOp ";")

signature :: Parser (LexInfo, Text, Type NextPass)
signature = "signature" <??> (\(ls, n) (le, t) -> (ls `mergeLexInfo` le, n, t))
    <$> ident 
    <*  reservedOp' "::"
    <*> typeP

signature' :: Parser (Text, Type NextPass)
signature' = (\(_, y, z) -> (y, z)) <$> signature

typeP :: Parser (LexInfo, Type NextPass)
typeP = "type" <??> constrained <|> unconstrained
    where
        constrained = do
            (ls, c) <- try $ constraint <* reservedOp' "=>"
            (\(le, t) -> (ls `mergeLexInfo` le, TConstraint c t))
                <$> unconstrained
        unconstrained = do
            (ls, t1) <- namedType
            functionType ls t1 
                <|> typeApp ls t1
                <|> pure (ls, t1)
        
            

constraint :: Parser (LexInfo, Constraint NextPass)
constraint = (\(ls, n) (le, t) -> (ls `mergeLexInfo` le, MkConstraint n t))
    <$> ident
    <*> typeP

namedType :: Parser (LexInfo, Type NextPass)
namedType = withParen typeP <|> do
    (li, i) <- ident
    pure $ if isLower (T.head $ T.takeWhileEnd (/='.') i)
        then (li, TVar (MkTVar i ()))
        else (li, TCon i ())

functionType :: LexInfo -> Type NextPass -> Parser (LexInfo, Type NextPass)
functionType li tyA = do
    reservedOp' "->"
    (le, tyB) <- typeP
    pure (li `mergeLexInfo` le, tyA -:> tyB)

typeApp :: LexInfo -> Type NextPass -> Parser (LexInfo, Type NextPass)
typeApp li tyA = do
    (le, tyB) <- typeP
    pure (li `mergeLexInfo` le, TApp tyA tyB)

withParen :: Parser a -> Parser a
withParen a = paren "(" *> a <* paren ")"

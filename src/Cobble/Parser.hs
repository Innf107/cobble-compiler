module Cobble.Parser where

import Cobble.Prelude.Parser hiding (assign)
import Cobble.Syntax
import Cobble.Syntax.PrettyPrint
import Cobble.Parser.Tokenizer (Token(..), TokenData(..))

import Data.Text qualified as T

import Data.Char

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
    
identWhere :: (Text -> Bool) -> Parser (LexInfo, Text)
identWhere pred = token' \case
    Token l (Ident t) | pred t -> Just (l, t)
    _ -> Nothing

exactIdent :: Text -> Parser LexInfo
exactIdent ident = token' \case
    Token li (Ident ident') | ident == ident' -> Just li
    _ -> Nothing

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
    
exactOp :: Text -> Parser LexInfo
exactOp op = token' \case
    Token li (Operator op') | op == op' -> Just li
    _ -> Nothing

reservedOp' :: Text -> Parser ()
reservedOp' = void . reservedOp
    
intLit :: Parser (LexInfo, Int)
intLit = (token' \case
    Token l (IntLiteral i) -> Just (l, i)
    _ -> Nothing) <?> "integer literal"

unitLit :: Parser LexInfo
unitLit = try (mergeLexInfo <$> paren "(" <*> paren ")")

lambdaE :: Parser (Expr NextPass)
lambdaE = "lambda expression" <??> (\ls xs e -> foldr (Lambda () (mergeLexInfo ls (getLexInfo e))) e xs)
    <$> reservedOp "\\"
    <*> many1 ident'
    <*  reservedOp' "->"
    <*> expr

handleE :: Parser (Expr NextPass)
handleE = ("handler expression" <??>) $ join $ (\ls expr handlers le -> separateHandlers handlers <&> \(mreturn, handlers) -> 
                                                                           Handle () (mergeLexInfo ls le) expr handlers mreturn)
    <$> reserved "handle"
    <*> expr
    <*  paren "{"
    <*> many (fmap Left effHandler <|> fmap Right returnHandler)
    <*> paren "}"
    where
        separateHandlers :: Seq (Either (EffHandler NextPass) (UnqualifiedName, Expr NextPass)) 
                         -> Parser (Maybe (UnqualifiedName, Expr NextPass), Seq (EffHandler NextPass))
        separateHandlers Empty = pure (Nothing, [])
        separateHandlers (Left effHandler :<| hs) = second (effHandler <|) <$> separateHandlers hs
        separateHandlers (Right returnHandler :<| hs) = do
            (mretHandler, effHandlers) <- separateHandlers hs
            case mretHandler of
                Nothing -> pure (Just returnHandler, effHandlers)
                Just h -> fail $ "Duplicate return clause in handler expression"


returnHandler :: Parser (UnqualifiedName, Expr NextPass)
returnHandler = (,)
    <$  reserved "return"
    <*> ident'
    <*  reservedOp "->"
    <*> expr
    <*  reservedOp ";"

effHandler :: Parser (EffHandler NextPass)
effHandler = (\(ls, c) xs e le -> EffHandler () (mergeLexInfo ls le) c xs e)
    <$> ident
    <*> many ident'
    <*  reservedOp "->"
    <*> expr
    <*> reservedOp ";"

letE :: Parser (Expr NextPass)
letE = "let binding" <??> (\ls n ps e b -> Let () (mergeLexInfo ls (getLexInfo e)) (Decl () n ps e) b)
    <$> reserved "let"
    <*> ident'
    <*> many ident'
    <*  reservedOp' "="
    <*> expr
    <*  reserved "in"
    <*> expr

module_ :: Parser (Module NextPass)
module_ = "module" <??> Module () <$> moduleDecl <*> statements <* eof
    where
        moduleDecl = reserved "module" *> (snd <$> modName) <* reservedOp ";"

statement :: Parser (Statement NextPass)
statement = "statement" <??> def <|> defVariant <|> defClass <|> defInstance <|> defEffect <|> import_

expr :: Parser (Expr NextPass)
expr = merge 
        <$> exprWithoutAscription 
        <*> optionMaybe ascription
    where
        ascription = "ascription" 
            <??>  
            reservedOp "::"
            *> typeP
        merge e (Just (le, ty)) = Ascription () (mergeLexInfo (getLexInfo e) le) e ty
        merge e Nothing = e

exprWithoutAscription :: Parser (Expr NextPass)
exprWithoutAscription = exprOrOp <&> \case
    OpLeaf e -> e
    opGroup  -> ExprX (Left opGroup) (leftLI opGroup `mergeLexInfo` rightLI opGroup)  
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
exprWithoutOp = resume <|> ("expression" <??> do
        f <- expr'
        args <- many expr'
        case args of
            Empty       -> pure f
            (a :<| as)  -> let li = (getLexInfo f `mergeLexInfo` (getLexInfo (last (a :| (toList as))))) in
                            pure $ foldl' (App () li) f (a :| (toList as)))

resume :: Parser (Expr NextPass)
resume = (\ls e -> Resume () (mergeLexInfo ls (getLexInfo e)) e)
    <$> reserved "resume"
    <*> expr'


expr' :: Parser (Expr NextPass)
expr' = "expression (no fcall)" <??> 
        uncurry (IntLit ()) <$> intLit 
    <|> ExprX (Right UnitLit) <$> unitLit 
    <|> lambdaE 
    <|> handleE
    <|> letE 
    <|> ifE 
    <|> caseE 
    <|> varOrConstr 
    <|> withParen expr


def :: Parser (Statement NextPass)
def = "definition" <??> do
    mfixity <- optionMaybe fixity
    (liStartSig, sigName, ty) <- signature_ 
    reservedOp' ";"

    defDecl@(Decl _ name _ e) <- decl

    when (name /= sigName) $ fail "Function definition does not immediately follow its type signature"
    
    pure $ Def (snd <$> mfixity) 
            (maybe liStartSig fst mfixity `mergeLexInfo` getLexInfo e) 
            defDecl ty

decl :: Parser (Decl NextPass)
decl = "declaration" <??> (\f xs e -> Decl () f xs e)
    <$> ident'
    <*> many ident'
    <* reservedOp' "="
    <*> expr

import_ :: Parser (Statement NextPass)
import_ = "import" <??> do
    liStart <- reserved "import"
    (liEnd, name) <- modName
    pure (Import () (liStart `mergeLexInfo` liEnd) name)

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
   
defVariant :: Parser (Statement NextPass)
defVariant = "variant definition" <??> (\ls n ps cs -> DefVariant () (ls `mergeLexInfo` snd (unsafeLast cs)) n ps (map (\((n,ts),_) -> (n, ts, ())) cs))
    <$> reserved "variant"
    <*> ident'
    <*> many tvarBinder
    <*  reservedOp' "="
    <*> (constr `sepBy1` reservedOp' "|")
    where
        constr = "variant constructor definition" <??> (\(ls, i) ts -> ((i, map snd ts), foldr (\x r -> fst x `mergeLexInfo` r) ls ts))
            <$> ident
            <*> many namedType

defClass :: Parser (Statement NextPass)
defClass = "class definition" <??> (\ls n ps cs le -> DefClass () (ls `mergeLexInfo` le) n ps cs)
    <$> reserved "class"
    <*> ident'
    <*> many tvarBinder
    <*  paren' "{"
    <*> many (signature' <* reservedOp' ";")
    <*> paren "}"

defInstance :: Parser (Statement NextPass)
defInstance = "instance definition" <??> (\ls cn (_, t) ds le -> DefInstance () (ls `mergeLexInfo` le) cn t ds)
    <$> reserved "instance"
    <*> ident'
    <*> typeP
    <*  paren' "{"
    <*> many (decl <* reservedOp' ";")
    <*> paren "}"

defEffect :: Parser (Statement NextPass)
defEffect = "effect definition" <??> (\ls effName tvs effs le -> DefEffect () (ls `mergeLexInfo` le) effName tvs effs)
    <$> reserved "effect"
    <*> ident'
    <*> many tvarBinder
    <*  paren' "{"
    <*> many (signature' <* reservedOp' ";")
    <*> paren "}"

ifE :: Parser (Expr NextPass)
ifE = "if expression" <??> (\liStart te ee -> If () (liStart `mergeLexInfo` (getLexInfo ee)) te ee)
    <$> reserved "if" <*> expr
    <*> (reserved "then" *> expr)
    <*> (reserved "else" *> expr)

caseE :: Parser (Expr NextPass)
caseE = "case expression" <??> (\ls e cases le -> Case () (ls `mergeLexInfo` le) e cases)
    <$> reserved "case"
    <*> expr
    <*  reserved "of"
    <*  paren' "{"
    <*> many caseBranch
    <*> paren "}"

caseBranch :: Parser (CaseBranch NextPass)
caseBranch = "case branch" <??> (\(p, ls) e le -> CaseBranch () (mergeLexInfo ls le) p e)
    <$> patternP
    <*  reservedOp' "->"
    <*> expr
    <*> reservedOp ";"


varOrConstr :: Parser (Expr NextPass)
varOrConstr = "variable or struct construction" <??> do
    (li, name) <- ident
    if isUpper (T.head name)
    then pure (VariantConstr () li name)
    else pure (Var () li name)

patternP :: Parser (Pattern NextPass, LexInfo)
patternP = parenPatternP `sepBy1` reservedOp' "|"
    <&> \case
        [p] -> p
        ps -> (OrP () (map fst ps), mergeLexInfo lstart lend)  
            where
                lstart = case ps of
                    ((_,ls):<|_) -> ls
                    _ -> error $ "patternP: sepBy1 returned empty list"
                lend =  case ps of
                    (_:|>(_, le)) -> le
                    _ -> error $ "patternP: sepBy1 returned empty list"

-- | Patterns that need additional parentheses when nested
parenPatternP :: Parser (Pattern NextPass, LexInfo)
parenPatternP = "pattern" <??> wildcardP <|> varOrConstrP <|> patternP'

patternP' :: Parser (Pattern NextPass, LexInfo)
patternP' = wildcardP <|> intP <|> withParen patternP

wildcardP :: Parser (Pattern NextPass, LexInfo)
wildcardP = "wildacrd pattern" <??> (\li -> (WildcardP (), li))
    <$> exactIdent "_"

intP :: Parser (Pattern NextPass, LexInfo)
intP = "integer pattern" <??> (\(li, n) -> (IntP () n, li))
    <$> intLit

{- note [(not . isUpper) vs isLower]
We have to use (not . isUpper) instead of isLower to parse variant constructors,
since "_" is neither an uppercase nor a lowercase character.
"_x" should obviously be a valid variable name (and therefore an invalid variant constructor name),
so we have to settle on (not . isUpper) instead of isLower.
-}

varOrConstrP :: Parser (Pattern NextPass, LexInfo)
varOrConstrP = do
    (ls, v) <- ident
    -- See note [(not . isUpper) vs. isLower]
    if not (isUpper (T.head v))
    then pure (VarP () v, ls)
    else do
        rest <- many (patternP' <|> varP)
        let le = case rest of
                [] -> ls
                _ -> snd (unsafeLast rest) in pure (ConstrP () v (map fst rest), mergeLexInfo ls le)
varP :: Parser (Pattern NextPass, LexInfo)
varP = do
    (ls, v) <- ident
    -- See note [(not . isUpper) vs. isLower]
    if isUpper (T.head v)
    then pure (ConstrP () v [], ls)
    else pure (VarP () v, ls)

statements :: Parser (Seq (Statement NextPass))
statements = many (statement <* reservedOp ";")

signature_ :: Parser (LexInfo, Text, UType)
signature_ = "signature" <??> (\(ls, n) (le, t) -> (ls `mergeLexInfo` le, n, t))
    <$> ident 
    <*  reservedOp' "::"
    <*> typeP

signature' :: Parser (Text, UType)
signature' = (\(_, y, z) -> (y, z)) <$> signature_

typeP' :: Parser UType
typeP' = snd <$> typeP

typeP :: Parser (LexInfo, UType)
typeP = "type" <??> forallTyP <|> constrained <|> unconstrained
    where
        forallTyP = (\ls tvs (le, ty) -> (mergeLexInfo ls le, UTForall tvs ty))
            <$> reserved "forall"
            <*> many tvarBinder
            <*  reservedOp' "."
            <*> typeP
        constrained = do
            (ls, c) <- try $ constraint <* reservedOp' "=>"
            (\(le, t) -> (ls `mergeLexInfo` le, UTConstraint c t))
                <$> unconstrained
        unconstrained = do
            (ls, t1) <- namedType
            restTys <- many namedType
            let ls' = case restTys of
                    [] -> ls
                    _ -> ls `mergeLexInfo` fst (unsafeLast restTys) 
            let t' = foldl' UTApp t1 (map snd restTys)
            functionType ls' t' 
                <|> pure (ls', t')

tvarBinder :: Parser (UnqualifiedName, Maybe Kind)
tvarBinder = (,Nothing) <$> ident'
          <|> withParen ((\x k -> (x, Just k))
                    <$> ident'
                    <*  reservedOp' "::"
                    <*> kindP)

kindP :: Parser Kind
kindP = do
    k1 <- kindP' 
    rest <- optionMaybe (reservedOp "->" *> kindP)
    pure $ case rest of
        Nothing -> k1
        Just k2 -> k1 `KFun` k2

kindP' :: Parser Kind
kindP' = withParen kindP <|> (KStar <$ exactOp "*")

constraint :: Parser (LexInfo, UConstraint)
constraint = (\(ls, n) (le, t) -> (ls `mergeLexInfo` le, MkUConstraint n t))
    <$> ident
    <*> typeP

namedType :: Parser (LexInfo, UType)
namedType = withParen typeP <|> do
    (li, i) <- ident
    pure $ if isLower (T.head $ T.takeWhileEnd (/='.') i)
        then (li, UTVar i)
        else (li, UTCon i)

functionType :: LexInfo -> UType -> Parser (LexInfo, UType)
functionType li tyA = simpleFunction <|> effFunction
    where
        simpleFunction = do
            reservedOp' "->"
            (le, tyB) <- typeP
            pure (li `mergeLexInfo` le, UTFun tyA (UTRowClosed Empty) tyB)
        effFunction = do
            exactOp "-"
            paren' "{"
            effRow <- effectRow
            paren' "}"
            exactOp ">"
            (le, tyB) <- typeP
            pure (li `mergeLexInfo` le, UTFun tyA effRow tyB)

effectRow :: Parser UType
effectRow = effVar <|> effRow 
    where
        effVar = UTVar . snd <$> identWhere (isLower . T.head)

        effRow = do
            tys <- typeP' `sepBy` reservedOp' ","
            mOpenVar <- optionMaybe (reservedOp' "|" *> ident')
            pure case mOpenVar of
                Nothing -> UTRowClosed tys
                Just var -> UTRowOpen tys var

withParen :: Parser a -> Parser a
withParen a = paren "(" *> a <* paren ")"

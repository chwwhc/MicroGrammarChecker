module JavaParser where
import AST
import Data.List ( sortBy )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Monad ( void )
import Text.ParserCombinators.Parsec.Language ( javaStyle )
import qualified Text.ParserCombinators.Parsec.Token as Token



keyWords :: [String]
keyWords = ["abstract", "continue", "for", "new", "switch",
    "assert", "default", "goto", "package", "synchronized",
    "boolean", "do", "if", "private", "this",
    "break", "double", "implements", "protected", "throw",
    "byte", "else", "import", "public", "throws",
    "case", "enum", "instanceof", "return", "transient",
    "catch", "extends", "int", "short", "try",
    "char", "final", "interface", "static", "void",
    "class", "finally", "long", "strictfp", "volatile",
    "const", "float", "native", "super", "while",
    "true", "false", "null" ]

modifiers :: [String]
modifiers = ["final", "static", "abstract", "transient", "synchronized",
            "volatile", "public", "private", "protected"]

keySymbols :: [String]
keySymbols = [ "*", "=", "||", "&&", "-", "+", "/", "&",
    ",", "(", ")", ">", "<", ">=", "<=", "!=",
    "[", "]", "{", "}", "#", "^", "~", "!", "%",
    "|", ".", "?", "==", ">>", "<<", "'", "\"",
    ":", "->", "[]", "+=", "-=", "*=", "/=", ";",
    "%=", "&=", "|=", "^=", ">>=", "<<=", "sizeof",
    "--", "++", "\\", "@" ]

languageDef = javaStyle {
    Token.nestedComments = False,
    Token.identStart = letter <|> char '_',
    Token.identLetter = alphaNum <|> char '_',
    Token.reservedNames = keyWords,
    Token.reservedOpNames = keySymbols,
    Token.caseSensitive = True
}

lexer = Token.makeTokenParser languageDef
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer
identifier :: Parser String
identifier = Token.identifier lexer
symbol :: String -> Parser String
symbol = Token.symbol lexer
integer :: Parser Integer
integer = Token.integer lexer <* skipMany letter
semi :: Parser String
semi = Token.semi lexer
comma :: Parser String
comma = Token.comma lexer
parens :: Parser a -> Parser a
parens = Token.parens lexer
braces :: Parser a -> Parser a
braces = Token.braces lexer
angles :: Parser a -> Parser a
angles = Token.angles lexer
brackets :: Parser a -> Parser a
brackets = Token.brackets lexer
reserved :: String -> Parser ()
reserved = Token.reserved lexer
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer
float :: Parser Double
float = Token.float lexer <* skipMany letter
charLiteral :: Parser Char
charLiteral = Token.charLiteral lexer
escape :: Parser String
escape = do { sl <- char '\\'; esch <- oneOf "\\\"0nrvtbf"; return [sl, esch]; }
nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"
singleStr :: Parser String
singleStr = return <$> nonEscape <|> escape
stringLiteral :: Parser String
stringLiteral = do { str <- many1 $ char '"' *> many singleStr <* char '"' <* whiteSpace; return $ concat $ concat str; }

parseWcStmt :: Stmt -> Stmt
parseWcStmt stmt = case stmt of
    IfStmt pos ex trBr Nothing -> IfStmt pos (head (parseExpr pos [ex])) (parseWcStmt trBr) Nothing
    IfStmt pos ex trBr (Just elBr) -> IfStmt pos (head (parseExpr pos [ex])) (parseWcStmt trBr) (Just (parseWcStmt elBr))
    WhileStmt pos ex st -> WhileStmt pos (head (parseExpr pos [ex])) (parseWcStmt st)
    DoWhileStmt pos st ex -> DoWhileStmt pos (parseWcStmt st) (head (parseExpr pos [ex]))
    ForStmt pos dec cond upd st -> ForStmt pos (head (parseExpr pos [dec])) (head (parseExpr pos [cond])) (head (parseExpr pos [upd])) (parseWcStmt st)
    SwitchStmt pos ex st -> SwitchStmt pos (head (parseExpr pos [ex])) (parseWcStmt st)
    CaseStmt pos ex st -> CaseStmt pos (head (parseExpr pos [ex])) (parseWcStmt st)
    DfltStmt pos st -> DfltStmt pos (parseWcStmt st)
    IterStmt pos dec up st -> IterStmt pos (head $ parseExpr pos [dec]) (head (parseExpr pos [up])) (parseWcStmt st)
    FnDecStmt pos name ex_lst st -> FnDecStmt pos name (parseExpr pos ex_lst) (parseWcStmt st)
    ReturnStmt pos ex -> ReturnStmt pos (head (parseExpr pos [ex]))
    LineStmt pos ex -> LineStmt pos (parseExpr pos ex)
    GotoStmt pos lab -> GotoStmt pos lab
    ClassStmt name st -> ClassStmt name (parseWcStmt st)
    TryStmt pos st -> TryStmt pos (parseWcStmt st)
    CatchStmt pos ex st -> CatchStmt pos (head (parseExpr pos [ex])) (parseWcStmt st)
    FinalStmt pos st -> FinalStmt pos (parseWcStmt st)
    BodyStmt st_lst -> BodyStmt (parseWcStmt <$> st_lst)
    other -> other

stmtParser :: Parser Stmt
stmtParser = try skipOther
    <|> try classParser
    <|> try fnDecParser
    <|> whileParser
    <|> ifParser
    <|> switchParser
    <|> caseParser
    <|> dfltParser
    <|> tryParser 
    <|> catchParser 
    <|> finParser
    <|> returnParser
    <|> forParser
    <|> contParser
    <|> brkParser
    <|> doWhileParser
    <|> lineParser
    <?> "any statement"

skipOther :: Parser Stmt
skipOther = try ((pRsrvdTbl ["@", "import", "package"]) >> skipMany (noneOf "\n\r") >> whiteSpace >> return OtherStmt)
            <|> try (pRsrvd "throw" >> skipTo ";" >> whiteSpace >> return OtherStmt)

lineParser :: Parser Stmt
lineParser = whiteSpace >> lookAhead (noneOf "{}") >> LineStmt <$> getPosition <*> ((:[]) . WildCard <$> skipTo ";") <?> "line"

returnParser :: Parser Stmt
returnParser = ReturnStmt <$> pRsrvd "return" <*> (WildCard <$> skipTo ";") <?> "return statement"

bodyParser :: Parser Stmt
bodyParser = BodyStmt <$> braces (manyTill stmtParser (lookAhead (symbol "}")))
        <|> (BodyStmt <$> ((:[]) <$> stmtParser)) <?> "compound statement"

ifParser :: Parser Stmt
ifParser = IfStmt <$> pRsrvd "if" <*> (WildCard <$> balancedP "(" ")") <*> bodyParser <*> optionMaybe elseParser <?> "if statement"

elseParser :: Parser Stmt
elseParser = pRsrvd "else" >> (ifParser <|> bodyParser) <?> "optional else statement"

whileParser :: Parser Stmt
whileParser = WhileStmt <$> pRsrvd "while" <*> (WildCard <$> balancedP "(" ")") <*> bodyParser <?> "while statement"

doWhileParser :: Parser Stmt
doWhileParser = DoWhileStmt <$> pRsrvd "do" <*> bodyParser <*> (WildCard <$> (reserved "while" >> balancedP "(" ")" <* semi)) <?> "do-while statement"

switchParser :: Parser Stmt
switchParser = SwitchStmt <$> pRsrvd "switch" <*> (WildCard <$> balancedP "(" ")") <*> bodyParser <?> "switch statement"

caseParser :: Parser Stmt
caseParser = try (CaseStmt <$> pRsrvd "case" <*> (WildCard <$> skipTo ":") <*> bodyParser)
            <|> try ( CaseStmt <$> pRsrvd "case" <*> (WildCard <$> skipTo ":") <*>
            (BodyStmt <$> manyTill stmtParser (lookAhead (caseParser <|> dfltParser))))
            <?> "case statement"

dfltParser :: Parser Stmt
dfltParser = try (DfltStmt <$> pRsrvd "default" <* symbol ":" <*> bodyParser)
            <|> try (DfltStmt <$> pRsrvd "default" <* symbol ":" <*>
            (BodyStmt <$> manyTill stmtParser (lookAhead (symbol "}"))))
            <?> "default statement"

forParser :: Parser Stmt
forParser = do {
    pos <- pRsrvd "for" <* symbol "(";
    tmp <- lookAhead (skipTo ")");
    if ":" `elem` tmp then iterParser pos
    else do { varDec <- skipTo ";";
        endCond <- skipTo ";";
        varUpdate <- skipTo ")";
        ForStmt pos (WildCard varDec) (WildCard endCond) (WildCard varUpdate) <$> bodyParser; };
} <?> "for statement"

tryParser :: Parser Stmt 
tryParser = TryStmt <$> pRsrvd "try" <*> bodyParser <?> "try statement"

catchParser :: Parser Stmt 
catchParser = CatchStmt <$> pRsrvd "catch" <*> (WildCard <$> balancedP "(" ")") <*> bodyParser <?> "catch statement"

finParser :: Parser Stmt 
finParser = FinalStmt <$> pRsrvd "finally" <*> bodyParser <?> "finally statement"

iterParser :: SourcePos -> Parser Stmt
iterParser pos = do {
    iterTp <- skipTo ":";
    iterVar <- skipTo ")";
    IterStmt pos (WildCard iterTp) (WildCard iterVar) <$> bodyParser;
}

contParser :: Parser Stmt
contParser = CntStmt <$> pRsrvd "continue" <* semi <?> "continue statement"

brkParser :: Parser Stmt
brkParser = BrkStmt <$> pRsrvd "break" <* semi <?> "break statement"

fnDecParser :: Parser Stmt
fnDecParser = do {
    pos <- skipMany (pRsrvdTbl modifiers) >> (try (typeParser >> getPosition) <|> getPosition);
    name <- identifier;
    args <- lookAhead (symbol "(") >> balancedP "(" ")" <* skipMany (reserved "throws" <|> void comma <|> void identifier <|> void space);
    try (symbol ";" >> return (FnDecStmt pos name [WildCard args] (BodyStmt [])))
    <|> FnDecStmt pos name [WildCard args] <$> bodyParser;
} <?> "function definition statement"

classParser :: Parser Stmt
classParser = do {
    name <- skipMany (pRsrvdTbl modifiers) >> (reserved "class" <|> reserved "interface") *> identifier <* (skipMany (pRsrvdTbl ["extends", "implements"]) >> identifier `sepBy` comma);
    ClassStmt name <$> bodyParser;
}

atomTpParser :: Parser Type
atomTpParser = parseTpGroup Long ["long"]
    <|> parseTpGroup Short ["short", "byte"]
    <|> parseTpGroup Char ["char"]
    <|> parseTpGroup Int ["int"]
    <|> parseTpGroup Bool ["boolean"]
    <|> parseTpGroup Float ["float"]
    <|> parseTpGroup Double ["double"]
    <|> parseTpGroup Void ["void"]
    <|> choice (try . strUnEnParser <$> ["enum"])
    <|> UnknownType <$> (try (identifier <* (lookAhead (angles (many (noneOf "&|()>"))) >> balancedP "<" ">") <* lookAhead identifier) <|> identifier <* lookAhead identifier)
    <?> "Atomic Type"
    where parseTpGroup tp tbl = choice (try . string <$> tbl) >> return tp

strUnEnParser :: String -> Parser Type
strUnEnParser s
  | s == "struct" = reserved s >> (Struct <$> identifier <|> return (Struct "anonymous"))
  | s == "union" = reserved s >> (Union <$> identifier <|> return (Union "anonymous"))
  | s == "enum" = reserved s >> (Enum <$> identifier <|> return (Enum "anonymous"))
  | otherwise = error "not one of struct, union, enum"

arrayParser :: Parser (Type -> Type)
arrayParser = whiteSpace >> brackets (skipMany (noneOf "]")) >> return Array <?> "array type"

typeParser :: Parser Type
typeParser = buildExpressionParser speTpTable (clear *> atomTpParser <* whiteSpace) <?> "type"
        where clear = whiteSpace >> skipMany (pRsrvdTbl modifiers)
              speTpTable = [[postfixChain arrayParser]]

exprParser :: SourcePos -> Parser [Expr]
exprParser pos = try (((string ". . ." >> return VoidExpr) <|> (typeParser >>= varDecParser pos)) `sepBy1` comma)
            <|> try (do{ tp <- typeParser; varDecParser pos tp `sepBy1` comma; })
            <|> try (exprParser' pos `sepBy1` comma)

parseExpr :: SourcePos -> [Expr] -> [Expr]
parseExpr pos wc =
    case wc of
        [WildCard wc'] ->
            case parse (exprParser pos) "" (unwords wc') of
            Right ex -> ex
            Left err -> [WildCard wc']
        _ -> wc

exprParser' :: SourcePos -> Parser Expr
exprParser' pos = whiteSpace *> (buildExpressionParser (opTable pos) (term pos) <|> return VoidExpr) <* whiteSpace <?> "expression"

varDecParser :: SourcePos -> Type -> Parser Expr
varDecParser pos tp = do { var <- exprParser' pos; return $ VarDec (tp, var); } <?> "variable declaration"

identParser :: SourcePos -> Parser Expr
identParser pos = setPosition pos >> Ident <$> getPosition <*> identifier <?> "identifier"

fnCallParser :: SourcePos -> Parser Expr
fnCallParser pos = do {
    name <- skipMany (reserved "new") >> (try (identifier <* (lookAhead (angles (many (noneOf "&|()>"))) >> balancedP "<" ">")) <|> identifier);
    symbol "(" >> (try (symbol ")" >> return (Call pos name [])) <|> Call pos name <$> exprParser' pos `sepBy` comma <* symbol ")");
}

atomParser :: SourcePos -> Parser Expr
atomParser pos = lexeme (try (nullParser pos)
    <|> try (floatParser pos)
    <|> try (boolParser pos)
    <|> try (intParser pos)
    <|> try (fnCallParser pos)
    <|> try (lstValParser pos)
    <|> try (identParser pos)
    <|> try (charParser pos)
    <|> try (strParser pos)
    <|> sizeOfParser )
    <?> "constant"

lstValParser :: SourcePos -> Parser Expr
lstValParser pos = Atom pos . ListVal <$> braces (atomParser pos `sepBy` comma) <?> "array"

nullParser :: SourcePos -> Parser Expr
nullParser pos = pRsrvd "null" >> return (Atom pos NullVal) <?> "null"

intParser :: SourcePos -> Parser Expr
intParser pos = Atom pos . IntVal <$> integer <?> "integer"

boolParser :: SourcePos -> Parser Expr
boolParser pos = (pRsrvd "true" >> return (Atom pos . BoolVal $ True)) <|> (pRsrvd "false" >> return (Atom pos . BoolVal $ False)) <?> "bool"

charParser :: SourcePos -> Parser Expr
charParser pos = Atom pos . CharVal <$> charLiteral <?> "char"

strParser :: SourcePos -> Parser Expr
strParser pos = Atom pos . StrVal <$> stringLiteral <?> "string"

floatParser :: SourcePos -> Parser Expr
floatParser pos = do {
    try (Atom pos . FloatVal <$> float)
    <|> do { symbol "-"; n <- float; return $ Atom pos $ FloatVal (-n); }
} <?> "floating point number"

sizeOfParser :: Parser Expr
sizeOfParser = SizeOf <$> (reserved "sizeof" >> parens typeParser) <?> "sizeof"

term :: SourcePos ->  Parser Expr
term pos = try $ parens (exprParser' pos) <|> atomParser pos

terOpParser :: Parser (Expr -> Expr)
terOpParser = do {
    pos <- getPosition;
    trBr <- symbol "?" >> term pos;
    elBr <- symbol ":" >> term pos;
    return $ \cond -> TernOp TerIfElse cond trBr elBr;
} <?> "ternary operation"

arrAccParser :: SourcePos -> Parser (Expr -> Expr)
arrAccParser pos = do { idx <- brackets (exprParser' pos); return $ \list -> ArrAcc pos list idx; } <?> "array accessing"

tpCastParser :: SourcePos -> Parser (Expr -> Expr)
tpCastParser pos = do { tp <- try $ parens typeParser; return $ \ex -> UnaOp (TypeCast tp) ex; } <?> "type casting"

binary name fun = Infix (reservedOp name >> return fun)
binary' name fun = Infix (do { pos <- getPosition; reservedOp name; return (fun pos)})
prefix name fun = Prefix (reservedOp name >> return fun)
postfix name fun = Postfix (reservedOp name >> return fun)
prefixChain p = Prefix . chainl1 p $ return (.)
postfixChain p = Postfix . chainl1 p $ return $ flip (.)
opTable :: SourcePos -> [[Operator Char () Expr]]
opTable pos = [ [postfix "++" (UnaOp PostInc),
        postfix "--" (UnaOp PostDec),
        prefixChain (tpCastParser pos),
        postfixChain (arrAccParser pos),
        binary' "." MemAcc AssocLeft],

        [prefix "++" (UnaOp PreInc),
        prefix "--" (UnaOp PreDec),
        prefix "+" (UnaOp Pos),
        prefix "-" (UnaOp Minus),
        prefix "!" (UnaOp Not),
        prefix "~" (UnaOp BitNeg)],

        [binary "*" (BinOp Mul) AssocLeft,
        binary "/" (BinOp Div) AssocLeft,
        binary "%" (BinOp Mod) AssocLeft],

        [binary "+" (BinOp Add) AssocLeft,
        binary "-" (BinOp Sub) AssocLeft],

        [binary ">>" (BinOp RShft) AssocLeft,
        binary "<<" (BinOp LShft) AssocLeft],

        [binary "<" (BinOp Lt) AssocLeft,
        binary "<=" (BinOp Lte) AssocLeft,
        binary ">" (BinOp Gt) AssocLeft,
        binary ">=" (BinOp Gte) AssocLeft],

        [binary "==" (BinOp Eq) AssocLeft,
        binary "!=" (BinOp Neq) AssocLeft],

        [binary "&" (BinOp BitAnd) AssocLeft],

        [binary "^" (BinOp BitXor) AssocLeft],

        [binary "|" (BinOp BitOr) AssocLeft],

        [binary "&&" (BinOp And) AssocLeft],

        [binary "||" (BinOp Or) AssocLeft],

        [postfixChain terOpParser],

        [binary "=" (Assign NoOpAssign) AssocRight,
        binary "+=" (Assign AddAssign) AssocRight,
        binary "-=" (Assign SubAssign) AssocRight,
        binary "*=" (Assign MulAssign) AssocRight,
        binary "/=" (Assign DivAssign) AssocRight,
        binary "%=" (Assign ModAssign) AssocRight,
        binary "<<=" (Assign LshftAssign) AssocRight,
        binary ">>=" (Assign RshftAssign) AssocRight,
        binary "&=" (Assign BitAndAssign) AssocRight,
        binary "^=" (Assign BitXorAssign) AssocRight,
        binary "|=" (Assign BitOrAssign) AssocRight] ]

skipTo :: String -> Parser [String]
skipTo p = try (eof >> error  ("cannot skip to " ++ p))
        <|> try (whiteSpace *> string p  <* whiteSpace >> return [])
        <|> (:) <$> anyTkn <*> skipTo p

pRsrvd :: String -> Parser SourcePos
pRsrvd rsrvd = getPosition <* reserved rsrvd

anyTkn :: Parser String
anyTkn = lexeme (try (do {
                name <- identifier;
                idx <- many1 (do { n <- brackets integer; return $ "[" ++ show n ++ "]"; });
                return $ name ++ concat idx; }) -- variable and array accessing
        <|> try identifier -- variable
        <|> try (do { str <- stringLiteral; return $ "\"" ++ str ++ "\""; }) -- string literal
        <|> try (do { ch <- charLiteral; return $ "\'" ++ [ch] ++ "\'"; }) -- char literal
        <|> try (do { neg <- symbol "-"; f <- float; return $ neg ++ show f }) -- negative float
        <|> try (fmap show float) -- positive float
        <|> try (pRsrvdTbl (sortBy tblDesc keySymbols)) -- reserved symbol
        <|> try (pRsrvdTbl (sortBy tblDesc keyWords)) -- reserved word
        <|> try (fmap show integer) -- integer
        <|> try (eof >> return "") -- to avoid the infinite recursion 
        )
        where tblDesc a b = compare (length b) (length a)

pRsrvdTbl :: [String] -> Parser String
pRsrvdTbl tbl = case tbl of
            [] -> fail "not in table"
            (x:xs) -> whiteSpace *> try (string x) <* whiteSpace <|> whiteSpace *> pRsrvdTbl xs <* whiteSpace

balancedP :: String -> String -> Parser [String]
balancedP l r = whiteSpace *> balancedP' l r 0 0 [] <* whiteSpace
            where balancedP' l r lcnt rcnt acc =
                    if lcnt == rcnt && lcnt /= 0 then
                        return $ (tail . init) acc
                    else
                        try (eof >> error ("cannot find balanced " ++ l ++ r))
                        <|> (symbol l >> balancedP' l r (lcnt + 1) rcnt (acc ++ [l]))
                        <|> try (symbol r >> balancedP' l r lcnt (rcnt + 1) (acc ++ [r]))
                        <|> try (do { tk <- anyTkn; balancedP' l r lcnt rcnt (acc ++ [tk]); })
                        <|> (semi >> balancedP' l r lcnt rcnt (acc ++ [";"]))

parseStmt :: String -> Stmt
parseStmt str = case parse stmtParser "" str of
    Left err -> error $ show err
    Right r -> parseWcStmt r

parseFile :: String -> IO [Stmt]
parseFile f = do {
    codes <- readFile f;
    case parse (whiteSpace >> manyTill (try (parseWcStmt <$> stmtParser)) eof) "" codes of
        Left e -> print e >> fail "failed to parse the source file"
        Right r -> return r
}
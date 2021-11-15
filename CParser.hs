module CParser where
import AST
import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

keyWords :: [String]
keyWords = [
        "auto", "break", "case", "char", "const", "if",
        "continue", "default", "do", "int", "long",
        "register", "return", "short", "signed", "static",
        "struct", "switch", "typedef", "union", "NULL",
        "unsigned", "void", "volatile", "while", "double",
        "else", "enum", "extern", "float", "for", "goto",
        "String", "string", "bool", "boolean", "public",
        "inline", "size_t"
            ]

keySymbols :: [String]
keySymbols = [
    "*", "=", "||", "&&", "-", "+", "/", "&",
    ",", "(", ")", ">", "<", ">=", "<=", "!=",
    "[", "]", "{", "}", "#", "^", "~", "!", "%",
    "|", ".", "?", "==", ">>", "<<", "'", "\"",
    ":", "->", "[]", "+=", "-=", "*=", "/=", ";",
    "%=", "&=", "|=", "^=", ">>=", "<<=", "sizeof",
    "--", "++"
        ]

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
integer = Token.integer lexer
semi :: Parser String
semi = Token.semi lexer
comma :: Parser String
comma = Token.comma lexer
parens :: Parser a -> Parser a
parens = Token.parens lexer
braces :: Parser a -> Parser a
braces = Token.braces lexer
brackets :: Parser a -> Parser a
brackets = Token.brackets lexer
reserved :: String -> Parser ()
reserved = Token.reserved lexer
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer
float :: Parser Double
float = Token.float lexer
charLiteral :: Parser Char
charLiteral = Token.charLiteral lexer
escape :: Parser String
escape = do {
    sl <- char '\\';
    esch <- oneOf "\\\"0nrvtbf";
    return [sl, esch];
}
nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"
singleStr :: Parser String
singleStr = return <$> nonEscape <|> escape
stringLiteral :: Parser String
stringLiteral = do { str <- char '"' *> many singleStr <* char '"'; return $ concat str; }

parseWcStmt :: Stmt -> Stmt
parseWcStmt stmt = case stmt of
    IfStmt pos ex trBr Nothing -> IfStmt pos (head (parseExpr [ex])) (parseWcStmt trBr) Nothing
    IfStmt pos ex trBr (Just elBr) -> IfStmt pos (head (parseExpr [ex])) (parseWcStmt trBr) (Just (parseWcStmt elBr))
    WhileStmt pos ex st -> WhileStmt pos (head (parseExpr [ex])) (parseWcStmt st)
    DoWhileStmt pos st ex -> DoWhileStmt pos (parseWcStmt st) (head (parseExpr [ex]))
    ForStmt pos dec cond upd st -> ForStmt pos (head $ parseExpr [dec]) (head (parseExpr [cond])) (head (parseExpr [upd])) (parseWcStmt st)
    SwitchStmt pos ex st -> SwitchStmt pos (head (parseExpr [ex])) (parseWcStmt st)
    CaseStmt pos ex st -> CaseStmt pos (head (parseExpr [ex])) (parseWcStmt st)
    DefaultStmt pos st -> DefaultStmt pos (parseWcStmt st)
    FnDecStmt pos name ex_lst st -> FnDecStmt pos name (parseExpr ex_lst) (parseWcStmt st)
    ReturnStmt ex -> ReturnStmt (head (parseExpr [ex]))
    LineStmt ex -> LineStmt (parseExpr ex)
    BodyStmt st_lst -> BodyStmt (parseWcStmt <$> st_lst)
    other -> other

stmtParser :: Parser Stmt
stmtParser = skipPrePros
    <|> try fnDecParser
    <|> try skipNewType
    <|> goToParser
    <|> whileParser
    <|> ifParser
    <|> switchParser
    <|> try labelParser
    <|> caseParser
    <|> dfltParser
    <|> returnParser
    <|> forParser
    <|> contParser
    <|> brkParser
    <|> doWhileParser
    <|> lineParser
    <?> "any statement"

skipNewType :: Parser Stmt
skipNewType = typeParser >> (lookAhead (symbol "{") >> balancedP "{" "}") >> lineParser >> return OtherStmt <?> "struct or union definition"

skipPrePros :: Parser Stmt
skipPrePros = try (symbol "#" >> skipMany1 (noneOf "{") >> balancedP "{" "}" >> skipMany1 (noneOf "\n\r") >> whiteSpace >> return OtherStmt)
            <|> (symbol "#" >> skipMany1 (noneOf "\n\r") >> whiteSpace >> return OtherStmt) 
            <?> "preprocessors"

lineParser :: Parser Stmt
lineParser = whiteSpace >> lookAhead (noneOf "{}") >> (LineStmt . (:[])) . WildCard <$> skipTo ";" <* whiteSpace <?> "line"

returnParser :: Parser Stmt
returnParser = ReturnStmt . WildCard <$> (reserved "return" *> skipTo ";" <* whiteSpace) <?> "return statement"

bodyParser :: Parser Stmt
bodyParser = BodyStmt <$> braces (manyTill stmtParser (lookAhead (symbol "}")))
        <|> (BodyStmt <$> ((:[]) <$> stmtParser)) <?> "compound statement"

ifParser :: Parser Stmt
ifParser = IfStmt <$> pRsrvd "if" <*> (WildCard <$> balancedP "(" ")") <*> bodyParser <*> optionMaybe elseParser <?> "if statement"

elseParser :: Parser Stmt
elseParser = reserved "else" >> (ifParser <|> bodyParser) <?> "optional else statement"

whileParser :: Parser Stmt
whileParser = WhileStmt <$> pRsrvd "while" <*> (WildCard <$> balancedP "(" ")") <*> bodyParser <?> "while statement"

goToParser :: Parser Stmt
goToParser = GotoStmt <$> pRsrvd "goto" <*> identifier <?> "goto statement"

doWhileParser :: Parser Stmt
doWhileParser = DoWhileStmt <$> pRsrvd "do" <*> bodyParser <*> (WildCard <$> (reserved "while" >> balancedP "(" ")" <* semi)) <?> "do-while statement"

switchParser :: Parser Stmt
switchParser = SwitchStmt <$> pRsrvd "switch" <*> (WildCard <$> balancedP "(" ")") <*> bodyParser <?> "switch statement"

caseParser :: Parser Stmt
caseParser = try (CaseStmt <$> pRsrvd "case" <*> (WildCard <$> skipTo ":" <* whiteSpace) <*> bodyParser)
            <|> try ( CaseStmt <$> pRsrvd "case" <*> (WildCard <$> skipTo ":" <* whiteSpace) <*>
            (BodyStmt <$> manyTill stmtParser (lookAhead (caseParser <|> dfltParser))))
            <?> "case statement"

dfltParser :: Parser Stmt
dfltParser = try (DefaultStmt <$> pRsrvd "default" <* symbol ":" <* whiteSpace <*> bodyParser)
            <|> try (DefaultStmt <$> pRsrvd "default" <* symbol ":" <* whiteSpace <*>
            (BodyStmt <$> manyTill stmtParser (lookAhead (symbol "}"))))
            <?> "default statement"

forParser :: Parser Stmt
forParser = do {
    pos <- pRsrvd "for" <* symbol "(";
    varDec <- skipTo ";";
    endCond <- skipTo ";";
    varUpdate <- skipTo ")" <* whiteSpace;
    ForStmt pos (WildCard varDec) (WildCard endCond) (WildCard varUpdate) <$> bodyParser;
} <?> "for statement"

labelParser :: Parser Stmt
labelParser = LabelStmt <$> (identifier <* symbol ":")

contParser :: Parser Stmt
contParser = reserved "continue" <* semi >> return ContStmt <?> "continue statement"

brkParser :: Parser Stmt
brkParser = reserved "break" <* semi >> return BrkStmt <?> "break statement"

fnDecParser :: Parser Stmt
fnDecParser = do {
    pos <- typeParser >> getPosition <* whiteSpace;
    name <- identifier <* whiteSpace;
    args <- lookAhead (symbol "(") >> balancedP "(" ")";
    try (symbol ";" >> return (FnDecStmt pos name [WildCard args] (BodyStmt [])))
    <|> FnDecStmt pos name [WildCard args] <$> bodyParser;
} <?> "function definition statement"

atomTpParser :: Parser Type
atomTpParser = parseTpGroup LongLong ["long long int", "long long", "signed long long int", "signed long long"]
    <|> parseTpGroup Long ["long int", "long", "signed long int", "signed long"]
    <|> parseTpGroup UnsignedLongLong ["unsigned long long int", "unsigned long long"]
    <|> parseTpGroup UnsignedLong  ["unsigned long int", "unsigned long"]
    <|> parseTpGroup Short ["short int", "short", "signed short int", "signed short"]
    <|> parseTpGroup UnsignedShort ["unsigned short int", "unsigned short"]
    <|> parseTpGroup Char ["signed char", "char"]
    <|> parseTpGroup Int ["int", "signed int", "signed", "size_t"]
    <|> parseTpGroup UnsignedInt ["unsigned int", "unsigned"]
    <|> parseTpGroup Bool ["bool"]
    <|> parseTpGroup Float ["float"]
    <|> parseTpGroup Double ["double"]
    <|> parseTpGroup LongDouble ["long double"]
    <|> parseTpGroup UnsignedChar ["unsigned char"]
    <|> parseTpGroup Void ["void"]
    <|> parseTpGroup Auto ["auto"]
    <|> choice (try . strUnEnParser <$> ["struct", "union", "enum"])
    <|> UnknownType <$> identifier <* lookAhead (identifier <|> symbol "*")
    <?> "Atomic Type"
    where parseTpGroup tp tbl = choice (try . string <$> tbl) >> return tp

strUnEnParser :: String -> Parser Type
strUnEnParser s
  | s == "struct" = reserved s >> (Struct <$> identifier <|> return (Struct "anonymous"))
  | s == "union" = reserved s >> (Union <$> identifier <|> return (Union "anonymous"))
  | s == "enum" = reserved s >> (Enum <$> identifier <|> return (Enum "anonymous"))
  | otherwise = error "not one of struct, union, enum"

ptrTpParser :: Parser (Type -> Type)
ptrTpParser = whiteSpace >> symbol "*" >> return Pointer <?> "pointer type"

refTpParser :: Parser (Type -> Type)
refTpParser = whiteSpace >> symbol "&" >> return Reference <?> "reference type"

typeParser :: Parser Type
typeParser = buildExpressionParser speTpTbl (skipMany (pRsrvdTbl ["static", "inline", "extern", "const", "typedef", "volatile", "register"]) *> atomTpParser <* whiteSpace) <?> "type"
    where speTpTbl = [[prefix "const" Const,
                    postfixChain ptrTpParser,
                    postfixChain refTpParser]]

exprParser :: Parser [Expr]
exprParser = try (((string ". . ." >> VoidExpr <$> getPosition) <|> (typeParser >>= varDecParser)) `sepBy1` comma)
            <|> try (do{ tp <- typeParser; varDecParser tp `sepBy1` comma; })
            <|> try (exprParser' `sepBy1` comma)

parseExpr :: [Expr] -> [Expr]
parseExpr wc =
    case wc of
        [WildCard wc'] ->
            case parse exprParser "" (unwords wc') of
            Right ex -> ex
            Left err -> [WildCard wc']
        _ -> wc

varDecParser :: Type -> Parser Expr
varDecParser tp = do { var <- exprParser'; return $ VarDec (tp, var); } <?> "variable declaration"

exprParser' :: Parser Expr
exprParser' = buildExpressionParser opTable term <|> VoidExpr <$> getPosition
            <?> "expression"

identParser :: Parser Expr
identParser = Ident <$> getPosition <*> identifier <?> "identifier"

fnCallParser :: Parser Expr
fnCallParser = do {
    pos <- getPosition;
    name <- identifier;
    symbol "(";
    try (symbol ")" >> return (Call pos name []))
    <|> Call pos name <$> exprParser' `sepBy` comma <* symbol ")";
}

atomParser :: Parser Expr
atomParser = try nullParser
    <|> try floatParser
    <|> try boolParser
    <|> try intParser
    <|> try fnCallParser
    <|> try lstValParser
    <|> try identParser
    <|> try charParser
    <|> try strParser
    <?> "constant"

lstValParser :: Parser Expr
lstValParser = do { pos <- getPosition; Atom pos . ListVal <$> braces (atomParser `sepBy` comma); } <?> "array"

nullParser :: Parser Expr
nullParser = do { pos <- getPosition <* reserved "NULL"; return $ Atom pos NullVal; } <?> "NULL"

intParser :: Parser Expr
intParser = do { pos <- getPosition; Atom pos . IntVal <$> integer; } <?> "integer"

boolParser :: Parser Expr
boolParser = do {
    pos <- getPosition;
    (reserved "true" >> return (Atom pos . BoolVal $ True)) <|> (reserved "false" >> return (Atom pos . BoolVal $ False))
} <?> "bool"

charParser :: Parser Expr
charParser = do { pos <- getPosition; Atom pos . CharVal <$> charLiteral; } <?> "char"

strParser :: Parser Expr
strParser = do { pos <- getPosition; Atom pos . StrVal <$> stringLiteral; } <?> "string"

floatParser :: Parser Expr
floatParser = do {
    pos <- getPosition;
    try (Atom pos . FloatVal <$> float)
    <|> do { symbol "-"; n <- float; return $ Atom pos $ FloatVal (-n); }
} <?> "floating point number"

term :: Parser Expr
term = try $ parens exprParser' <|> atomParser

terOpParser :: Parser (Expr -> Expr)
terOpParser = do {
    pos <- getPosition;
    trBr <- symbol "?" *> term;
    elBr <- symbol ":" *> term;
    return $ \cond -> TernOp TerIfElse cond trBr elBr;
} <?> "ternary operation"

arrAccParser :: Parser (Expr -> Expr)
arrAccParser = do {
    pos <- getPosition;
    idx <- brackets exprParser';
    return $ \list -> ArrAcc pos list idx
} <?> "array accessing"

tpCastParser :: Parser (Expr -> Expr)
tpCastParser = do {
    pos <- getPosition;
    tp <- try $ parens typeParser;
    return $ \ex -> UnaOp (TypeCast tp) ex
} <?> "type casting"

binary name fun = Infix (reservedOp name >> return fun)
binary' name fun = Infix (do { pos <- getPosition; reservedOp name; return (fun pos)})
prefix name fun = Prefix (reservedOp name >> return fun)
postfix name fun = Postfix (reservedOp name >> return fun)
prefixChain p = Prefix . chainl1 p $ return (.)
postfixChain p = Postfix . chainl1 p $ return $ flip (.)
opTable :: [[Operator Char () Expr]]
opTable = [
        [postfix "++" (UnaOp PostInc),
        postfix "--" (UnaOp PostDec),
        prefixChain tpCastParser,
        postfixChain arrAccParser,
        binary' "." MemAcc AssocLeft,
        binary' "->" PtrMemAcc AssocLeft],

        [prefix "++" (UnaOp PreInc),
        prefix "--" (UnaOp PreDec),
        prefix "+" (UnaOp Pos),
        prefix "-" (UnaOp Minus),
        prefix "!" (UnaOp Not),
        prefix "~" (UnaOp BitNeg),
        prefix "*" (UnaOp DeRef),
        prefix "&" (UnaOp Addr),
        prefix "sizeof" (UnaOp SizeOf)],

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
        binary "|=" (Assign BitOrAssign) AssocRight]
    ]

skipTo :: String -> Parser [String]
skipTo p = try (eof >> error  ("cannot skip to " ++ p))
        <|> try (whiteSpace >> string p  <* whiteSpace >> return [])
        <|> (:) <$> anyTkn <*> skipTo p

pRsrvd :: String -> Parser SourcePos
pRsrvd rsrvd = getPosition <* reserved rsrvd

anyTkn :: Parser String
anyTkn = whiteSpace >> lexeme (try (do {
                name <- identifier;
                idx <- many1 (do { n <- brackets integer; return $ "[" ++ show n ++ "]"; });
                return $ name ++ concat idx
                }) -- variable and array accessing
        <|> try identifier -- variable
        <|> try (do { rWrd <- pRsrvdTbl (sortBy tblDesc keyWords);
                ptrs <- manyTill (symbol "*") (lookAhead identifier);
                return $ rWrd ++ concat ptrs; }) -- pointer type
        <|> try (do { str <- stringLiteral; return $ "\"" ++ str ++ "\""; }) -- string literal
        <|> try (do { ch <- charLiteral; return $ "\'" ++ [ch] ++ "\'"; }) -- char literal
        <|> try (do { neg <- symbol "-"; f <- float; return $ neg ++ show f }) -- negative float
        <|> try (fmap show float) -- positive float
        <|> try (pRsrvdTbl (sortBy tblDesc keySymbols)) -- reserved symbol
        <|> try (pRsrvdTbl (sortBy tblDesc keyWords)) -- reserved word
        <|> try (fmap show integer) -- integer
        <|> try (eof >> string "") -- to avoid the infinite recursion 
        )
        where tblDesc a b = compare (length b) (length a)

pRsrvdTbl :: [String] -> Parser String
pRsrvdTbl tbl = case tbl of
            [] -> fail "not in table"
            (x:xs) -> try (string x) <* whiteSpace <|> pRsrvdTbl xs <* whiteSpace

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
module Parser where
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
        "struct", "switch", "typedef", "union",
        "unsigned", "void", "volatile", "while", "double",
        "else", "enum", "extern", "float", "for", "goto",
        "String", "string", "bool", "boolean", "public"
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
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer
parens :: Parser a -> Parser a
parens = Token.parens lexer
braces :: Parser a -> Parser a
braces = Token.braces lexer
brackets :: Parser a -> Parser a
brackets = Token.brackets lexer
reserved :: String -> Parser ()
reserved = Token.reserved lexer
float :: Parser Double
float = Token.float lexer
charLiteral :: Parser Char
charLiteral = Token.charLiteral lexer
stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

parseWcStmt :: Stmt -> Stmt
parseWcStmt stmt = case stmt of
    IfStmt pos ex trBr Nothing -> IfStmt pos (exprParser ex) (parseWcStmt trBr) Nothing
    IfStmt pos ex trBr (Just elBr) -> IfStmt pos (exprParser ex) (parseWcStmt trBr) (Just (parseWcStmt elBr))
    WhileStmt pos ex st -> WhileStmt pos (exprParser ex) (parseWcStmt st)
    DoWhileStmt pos st ex -> DoWhileStmt pos (parseWcStmt st) (exprParser ex)
    ForStmt pos dec cond upd st -> ForStmt pos (exprParser dec) (exprParser cond) (exprParser upd) (parseWcStmt st)
    SwitchStmt pos ex st -> SwitchStmt pos (exprParser ex) (parseWcStmt st)
    CaseStmt pos ex st -> CaseStmt pos (exprParser ex) (parseWcStmt st)
    DefaultStmt pos st -> DefaultStmt pos (parseWcStmt st)
    FnDecStmt pos name ex_lst st -> FnDecStmt pos name (argsParser ex_lst) (parseWcStmt st)
    ReturnStmt ex -> ReturnStmt (exprParser ex)
    LineStmt ex -> LineStmt (multExprParser ex)
    BodyStmt st_lst -> BodyStmt (parseWcStmt <$> st_lst)
    UnknownStmt pos Nothing -> UnknownStmt pos Nothing
    UnknownStmt pos (Just st) -> UnknownStmt pos (Just (parseWcStmt st))
    other -> other

-- not complete
stmtParser :: Parser Stmt
stmtParser = skipPrePros
    <|> whileParser
    <|> ifParser
    <|> try fnDecParser
    <|> switchParser
    <|> caseParser
    <|> dfltParser
    <|> returnParser
    <|> forParser
    <|> contParser
    <|> brkParser
    <|> doWhileParser
    <|> try lineParser
    <|> try unknwnParser
    <?> "any statement"

lineParser :: Parser Stmt
lineParser = do {
        whiteSpace >> lookAhead (noneOf "(){}[]");
        ex <- (try (skipTo ";") <|> try (skipTo ":")) <* whiteSpace;
        return $ LineStmt [WildCard ex]
        }

unknwnParser :: Parser Stmt
unknwnParser = do {
    pos <- getPosition <* skipMany1 (noneOf "{}");
    UnknownStmt pos <$> optionMaybe bodyParser;
}

returnParser :: Parser Stmt
returnParser = ReturnStmt . WildCard <$> (reserved "return" *> skipTo ";") <?> "return statement"

bodyParser :: Parser Stmt
bodyParser = try (BodyStmt <$> braces (many stmtParser) <* whiteSpace)
        <|> try (BodyStmt <$> ((:[]) <$> stmtParser) <* whiteSpace) <?> "compound statement"

ifParser :: Parser Stmt
ifParser = do {
    pos <- pRsrvd "if";
    cond <- lookAhead (symbol "(") >> balancedP;
    trBr <- bodyParser;
    IfStmt pos (WildCard cond) trBr <$> optionMaybe elseParser;
} <?> "if statement"

elseParser :: Parser Stmt
elseParser = reserved "else" >> (ifParser <|> bodyParser) <?> "optional else statement"

whileParser :: Parser Stmt
whileParser = do {
    pos <- pRsrvd "while";
    ex <- lookAhead (symbol "(") >> balancedP;
    WhileStmt pos (WildCard ex) <$> bodyParser;
} <?> "while statement"

doWhileParser :: Parser Stmt
doWhileParser = do {
    pos <- pRsrvd "do";
    body <- bodyParser;
    ex <- reserved "while" >> lookAhead (symbol "(") >> balancedP <* semi;
    return $ DoWhileStmt pos body (WildCard ex);
} <?> "do-while statement"

switchParser :: Parser Stmt
switchParser = do {
    pos <- pRsrvd "switch";
    ex <- lookAhead (symbol "(") >> balancedP;
    SwitchStmt pos (WildCard ex) <$> bodyParser;
} <?> "switch statement"

caseParser :: Parser Stmt
caseParser = do {
    pos <- pRsrvd "case";
    ex <- skipTo ":";
    stmts <- manyTill stmtParser (lookAhead (caseParser <|> dfltParser));
    return $ CaseStmt pos (WildCard ex) (BodyStmt stmts);
} <?> "case statement"

dfltParser :: Parser Stmt
dfltParser = do {
    pos <- pRsrvd "default" <* reservedOp ":";
    stmts <- manyTill stmtParser (lookAhead (symbol "}"));
    return $ DefaultStmt pos (BodyStmt stmts);
} <?> "default statement"

forParser :: Parser Stmt
forParser = do {
    pos <- pRsrvd "for" <* symbol "(";
    varDec <- skipTo ";";
    endCond <- skipTo ";";
    varUpdate <- skipTo ")" <* whiteSpace;
    ForStmt pos (WildCard varDec) (WildCard endCond) (WildCard varUpdate) <$> bodyParser;
} <?> "for statement"

contParser :: Parser Stmt
contParser = (reserved "continue" <* semi) >> return ContStmt <?> "continue statement"

brkParser :: Parser Stmt
brkParser = (reserved "break" <* semi) >> return BrkStmt <?> "break statement"

fnDecParser :: Parser Stmt
fnDecParser = do {
    pos <- manyTill anyToken space >> getPosition <* whiteSpace;
    name <- identifier <* whiteSpace;
    args <- lookAhead (symbol "(") >> balancedP;
    FnDecStmt pos name [WildCard args] <$> bodyParser;
} <?> "function definition statement"

atomTpParser :: Parser Type
atomTpParser = (reserved "int" >> return Int)
    <|> (reserved "char" >> return Char)
    <|> (reserved "float" >> return Float)
    <|> (reserved "double" >> return Double)
    <|> (choice [reserved "bool", reserved "boolean"] >> return Bool)
    <|> (reserved "void" >> return Void)
    <|> (reserved "string" >> return (Pointer Char))
    <|> strUnParser "struct"
    <|> strUnParser "union"
    <|> try (UnknownType <$> identifier <* lookAhead identifier)
    <?> "Atomic Type"

strUnParser :: String -> Parser Type
strUnParser s = reserved s >> Struct <$> identifier;

ptrTpParser :: Parser (Type -> Type)
ptrTpParser = do {
    whiteSpace *> symbol "*";
    return $ \t -> Pointer t;
} <?> "pointer"

refTpParser :: Parser (Type -> Type)
refTpParser = do {
    whiteSpace *> symbol "&";
    return $ \t -> Reference t;
} <?> "reference"

typeParser :: Parser Type
typeParser = buildExpressionParser tpTbl atomTpParser <?> "type"
    where tpTbl = [[prefix "const" Const,
                    postfixChain ptrTpParser,
                    postfixChain refTpParser]]

multExprParser' :: Parser [Expr]
multExprParser' = try (do { tp <- typeParser; varDecParser tp `sepBy` comma;})
            <|> try (exprParser' `sepBy` comma)

multExprParser :: [Expr] -> [Expr]
multExprParser [WildCard wc] =
    case parse multExprParser' "" (unwords wc) of
        Right ex -> ex
        Left err -> [WildCard wc]
multExprParser other = other

exprParser :: Expr -> Expr
exprParser (WildCard wc) = case parse exprParser' "" (unwords wc) of
    Right ex -> ex
    Left err -> WildCard wc
exprParser other = other

argsParser :: [Expr] -> [Expr]
argsParser [WildCard wc] = case parse ((typeParser >>= varDecParser) `sepBy` comma) "" (unwords wc) of
  Right ex -> ex
  Left err -> [WildCard wc]

varDecParser :: Type -> Parser Expr
varDecParser tp = do { var <- exprParser'; return $ VarDec (tp, var); }

exprParser' :: Parser Expr
exprParser' = buildExpressionParser opTable term
    <?> "expression"

identParser :: Parser Expr
identParser = do {
    pos <- getPosition;
    Ident pos <$> identifier;
}

fnCallParser :: Parser Expr
fnCallParser = do {
    pos <- getPosition;
    name <- identifier;
    symbol "(";
    try (do {
        symbol ")";
        return $ Call pos name [];
    })
    <|> (do {
    ex <- exprParser' `sepBy` comma;
    symbol ")";
    return $ Call pos name ex;
    }
    )
}

atomParser :: Parser Expr
atomParser = try floatParser
    <|> try boolParser
    <|> try charParser
    <|> try strParser
    <|> try intParser
    <|> try fnCallParser
    <|> try lstValParser
    <|> identParser
    <?> "constant"

lstValParser :: Parser Expr
lstValParser = do {
    pos <- getPosition;
    Atom pos . ListVal <$> braces (atomParser `sepBy` comma);
}<?> "array"

intParser :: Parser Expr
intParser = do {
    pos <- getPosition;
    Atom pos . IntVal <$> integer
}<?> "integer"

boolParser :: Parser Expr
boolParser = do {
    pos <- getPosition;
    (reserved "true" >> return (Atom pos . BoolVal $ True))
    <|> (reserved "false" >> return (Atom pos . BoolVal $ False))
}<?> "bool"

charParser :: Parser Expr
charParser = do {
    pos <- getPosition;
    Atom pos . CharVal <$> between (symbol "\'") (symbol "\'") anyChar;
} <?> "char"

strParser :: Parser Expr
strParser = do {
    pos <- getPosition;
    Atom pos . StrVal <$> between (symbol "\"") (symbol "\"") (manyTill anyChar (lookAhead $ symbol "\""));
} <?> "string"

floatParser :: Parser Expr
floatParser = do {
    pos <- getPosition;
    try (Atom pos . FloatVal <$> float)
    <|> do {
        symbol "-";
        n <- float;
        return $ Atom pos $ FloatVal (-n);
    }
}<?> "floating point number"

term :: Parser Expr
term = try atomParser
    <|> parens exprParser'

terOpParser :: Parser (Expr -> Expr)
terOpParser = do {
    pos <- getPosition;
    trBr <- reservedOp "?" *> term;
    elBr <- reservedOp ":" *> term;
    return $ \cond -> TernOp TerIfElse cond trBr elBr;
}

arrAccParser :: Parser (Expr -> Expr)
arrAccParser = do {
    pos <- getPosition;
    idx <- brackets exprParser';
    return $ \list -> ArrAcc pos list idx
}

tpCastParser :: Parser (Expr -> Expr)
tpCastParser = do {
    pos <- getPosition;
    tp <- try $ parens typeParser;
    return $ \ex -> UnaOp (TypeCast tp) ex
}

binary name fun = Infix (do{ reservedOp name; return fun})
binary' name fun = Infix (do { pos <- getPosition; reservedOp name; return (fun pos)})
prefix name fun = Prefix (do{ reservedOp name; return fun})
postfix name fun = Postfix (do{ reservedOp name; return fun})
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

skipPrePros :: Parser Stmt
skipPrePros = do {
    reservedOp "#" >> skipMany1 (noneOf "\n\r") >> whiteSpace;
    return PreProsStmt;
}

parseStmt :: String -> Stmt
parseStmt str = case parse stmtParser "" str of
    Left err -> error $ show err
    Right r -> parseWcStmt r

parseFile :: String -> IO [Stmt]
parseFile f = do {
    codes <- readFile f;
    case parse (whiteSpace *> manyTill (try (parseWcStmt <$> stmtParser)) eof) "" codes of
        Left e -> print e >> fail "failed to parse the source file"
        Right r -> return r
}

skipTo :: String -> Parser [String]
skipTo p = try (string p >> return [])
        <|> (:) <$> anyTkn <*> skipTo p

pRsrvd :: String -> Parser SourcePos
pRsrvd rsrvd = getPosition <* reserved rsrvd

{--
Not complete
--}
anyTkn :: Parser String
anyTkn = whiteSpace >> lexeme (try (do {
                name <- identifier;
                idx <- many1 (do {
                    l <- symbol "[";
                    n <- integer;
                    r <- symbol "]";
                    return $ l ++ show n ++ r;
                });
                return $ name ++ concat idx
                }) -- variable and array accessing
        <|> try identifier -- variable
        <|> try (do {
                rWrd <- parseRsrvd (sortBy (\e1 e2 -> compare (length e2) (length e1)) keyWords);
                ptrs <- manyTill (symbol "*") (lookAhead identifier);
                return $ rWrd ++ concat ptrs
                }) -- pointer type
        <|> try (do{
                l <- (:[]) <$> char '\"';
                s <- manyTill anyChar (lookAhead $ char '\"');
                r <- (:[]) <$> char '\"';
                return $ l ++ s ++ r
                }) -- string literal
        <|> try (do{
                l <- (:[]) <$> char '\'';
                s <- manyTill anyChar (lookAhead $ char '\'');
                r <- (:[]) <$> char '\'';
                return $ l ++ s ++ r
                }) -- char literal
        <|> try (do {
                neg <- symbol "-";
                f <- float;
                return $ neg ++ show f
                }) -- negative float
        <|> try (fmap show float) -- positive float
        <|> try (parseRsrvd (sortBy (\e1 e2 -> compare (length e2) (length e1)) keySymbols)) -- reserved symbol
        <|> try (parseRsrvd (sortBy (\e1 e2 -> compare (length e2) (length e1)) keyWords)) -- reserved word
        <|> try (fmap show integer) -- integer
        -- non-terminating when parsing " " caused by the next line 
        <|> try (eof >> error "end of file")
        <|> try (manyTill anyToken (lookAhead anyTkn)) -- last resort: any token
        )
    where parseRsrvd tbl = case tbl of
            [] -> fail "not in table"
            (x:xs) -> try (string x) <|> parseRsrvd xs

balancedP :: Parser [String]
balancedP = whiteSpace *> balancedP' 0 0 [] <* whiteSpace
    where balancedP' lcnt rcnt acc =
            if lcnt == rcnt && lcnt /= 0 then
                return $ (tail . init) acc
            else
                try (symbol "(" >> balancedP' (lcnt + 1) rcnt (acc ++ ["("]))
                <|> try (symbol ")" >> balancedP' lcnt (rcnt + 1) (acc ++ [")"]))
                <|> try (do {
                    tk <- anyTkn;
                    balancedP' lcnt rcnt (acc ++ [tk]);
                })
                <|> do {
                    semi >> balancedP' lcnt rcnt (acc ++ [";"]);
                }
module Parser where
import AST
import Control.Monad
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
        "String", "string", "bool", "boolean"
            ]

keySymbols :: [String]
keySymbols = [
    "*", "=", "||", "&&", "-", "+", "/", "&",
    ",", "(", ")", ">", "<", ">=", "<=", "!=",
    "[", "]", "{", "}", "#", "^", "~", "!", "%",
    "|", ".", "?", "==", ">>", "<<", "'", "\"",
    ":", "->", "[]", "+=", "-=", "*=", "/=", ";",
    "%=", "&=", "|=", "^=", ">>=", "<<=", "sizeof"
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

stmtParser :: Parser Stmt
stmtParser = skipPrePros
    <|> whileParser
    <|> ifParser
    -- <|> try fnDefParser
    <|> switchParser
    <|> caseParser
    <|> dfltParser
    <|> returnParser
    <|> forParser
    <|> contParser
    <|> brkParser
    <|> doWhileParser
    <|> try blockParser
    <|> try (LineStmt . WildCard <$> skipTo ";")
    <?> "statement"

lnParser :: Parser Stmt
lnParser = whiteSpace *> (LineStmt . WildCard <$> skipTo ";") <* whiteSpace

returnParser :: Parser Stmt
returnParser = do {
    ex <- reserved "return" >> skipTo ";";
    return $ ReturnStmt (WildCard ex);
} <?> "return statement"

blockParser :: Parser Stmt
blockParser = do {
    BlockStmt <$> braces (many stmtParser);
} <?> "compound statement"

ifParser :: Parser Stmt
ifParser = do {
    pos <- pRsrvd "if";
    cond <- balancedP;
    trBr <- blockParser;
    IfStmt pos (WildCard cond) trBr <$> optionMaybe elseParser;
}

elseParser :: Parser Stmt
elseParser = reserved "else" >> (ifParser <|> blockParser)

whileParser :: Parser Stmt
whileParser = do {
    pos <- pRsrvd "while";
    wc <- balancedP;
    WhileStmt pos (WildCard wc) <$> blockParser;
} <?> "while statement"

doWhileParser :: Parser Stmt
doWhileParser = do {
    pos <- pRsrvd "do";
    body <- blockParser;
    wc <- reserved "while" >> balancedP <* semi;
    return $ DoWhileStmt pos body (WildCard wc);
} <?> "do-while statement"

switchParser :: Parser Stmt
switchParser = do {
    pos <- pRsrvd "switch";
    ex <- balancedP;
    SwitchStmt pos (WildCard ex) <$> blockParser;
}

-- The body stmt part is not implemented
caseParser :: Parser Stmt
caseParser = do {
    pos <- pRsrvd "case";
    ex <- skipTo ":";
    stmts <- manyTill lnParser (lookAhead (caseParser <|> dfltParser));
    return $ CaseStmt pos (WildCard ex) (BlockStmt stmts);
}

dfltParser :: Parser Stmt
dfltParser = do {
    pos <- pRsrvd "default" <* reservedOp ":";
    stmts <- manyTill lnParser (lookAhead (symbol "}"));
    return $ DefaultStmt pos (BlockStmt stmts);
}

forParser :: Parser Stmt
forParser = do {
    pos <- pRsrvd "for" <* symbol "(";
    varDec <- skipTo ";";
    endCond <- skipTo ";";
    varUpdate <- skipTo ")" <* whiteSpace;
    ForStmt pos (WildCard varDec) (WildCard endCond) (WildCard varUpdate) <$> blockParser;
} <?> "for statement"

contParser :: Parser Stmt
contParser = do {
    reserved "continue" <* semi;
    return ContStmt;
} <?> "continue statement"

brkParser :: Parser Stmt
brkParser = do {
    reserved "break" <* semi;
    return BrkStmt;
} <?> "break statement"

{--
fnDefParser :: Parser Dec
fnDefParser = do {
    name <- anyTkn >> anyTkn;
    symbol "(";
    try (do {
        symbol ")";
        FnDec name (VarDec []) <$> blockParser;
    })
    <|> (do {
    para <- (typeParser *> identParser <* skipMany (oneOf "[]")) `sepBy` comma;
    symbol ")";
    FnDefStmt tp name para <$> blockParser;
    })
}
--}

-- Not complete
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
    <|> Unknown <$> anyTkn
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
typeParser = buildExpressionParser tpTbl atomTpParser
    where tpTbl = [[prefix "const" Const,
                    postfixChain ptrTpParser,
                    postfixChain refTpParser]]

exprParser :: Parser Expr
exprParser = buildExpressionParser opTable term
    <?> "operator expression"

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
        return $ FnCall pos name [];
    })
    <|> (do {
    ex <- exprParser `sepBy` comma;
    symbol ")";
    return $ FnCall pos name ex;
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
}

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
term = atomParser
    <|> parens exprParser

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
    idx <- brackets exprParser;
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
    Left e -> error $ show e
    Right r -> r

parseFile :: String -> IO [Stmt]
parseFile f = do {
    codes <- readFile f;
    case parse (whiteSpace *> manyTill stmtParser eof) "" codes of
        Left e -> print e >> fail "failed to parse the source file"
        Right r -> return r
}

skipTo :: String -> Parser [String]
skipTo p = (string p >> return [])
        <|> (:) <$> anyTkn <*> skipTo p

anyTkn :: Parser String
anyTkn = do {
    whiteSpace;
    wc <- lexeme (try identifier
        <|> try (parseRsrvd keySymbols)
        <|> try (parseRsrvd keyWords)
        <|> try stringLiteral
        <|> try (fmap (: []) charLiteral)
        <|> try (fmap show integer));
    whiteSpace;
    return wc;
}

parseRsrvd :: [String] -> Parser String
parseRsrvd tbl = case tbl of
    [] -> fail "not in table"
    (x:xs) -> try (string x) <|> parseRsrvd xs

pRsrvd :: String -> Parser SourcePos 
pRsrvd rsrvd = getPosition <* reserved rsrvd

balancedP :: Parser [String]
balancedP = balancedP' 0 0 []
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
    
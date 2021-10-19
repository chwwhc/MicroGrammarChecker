module Parser where
import MicroGrammar
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

keyWords :: [String]
keyWords = [
        "auto", "break", "case", "char", "const", "if",
        "continue", "default", "do", "int", "long",
        "register", "return", "short", "signed", "sizeof",
        "static", "struct", "switch", "typedef", "union",
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
    ":", "->", "[]"
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

-- Not complete
atomTpParser :: Parser Type
atomTpParser = (reserved "int" >> return Int)
    <|> (reserved "char" >> return Char)
    <|> (reserved "float" >> return Float)
    <|> (reserved "double" >> return Double)
    <|> (choice [reserved "bool", reserved "boolean"] >> return Bool)
    <|> (reserved "void" >> return Void)
    <|> (reserved "string" >> return (Pointer Char))
    <?> "Atomic Type"

ptrTpParser :: Type -> Parser Type
ptrTpParser tp = do {
    acc <- many1 $ whiteSpace *> char '*';
    return $ foldl (\t len -> Pointer t) tp acc;
} <?> "pointer"

-- typeParser succeded with "int****", but varDecParser failed
typeParser :: Parser Type
typeParser = try ( do {
        tp <- atomTpParser;
        ptrTpParser tp;
    })
    <|> atomTpParser
    <?> "type"

stmtParser :: Parser Stmt
stmtParser = try whileParser
    <|> try ifParser
    <|> try fnDefParser
    <|> try switchParser
    <|> try returnParser
    <|> try forParser
    <|> try contParser
    <|> try brkParser
    <|> try doWhileParser
    <|> try lnParser
    <|> try blockParser
    -- <|> try wildCardParser
    <?> "statement"

assignParser :: Parser Stmt
assignParser = do {
    pos <- getPosition;
    name <- exprParser;
    reservedOp "=";
    ex <- exprParser;
    return $ AssignStmt pos (name, ex);
}

exprStmtParser :: Parser Stmt
exprStmtParser = do {
    pos <- getPosition;
    ex <- exprParser;
    try (do {
        semi;
        return $ ExprStmt pos ex;
    })
    <|> ExprStmt pos <$> exprParser;
} <?> "expression statement"

-- Cannot parse "int x = 123, y;", should add wildCard
lnParser :: Parser Stmt
lnParser = do {
    pos <- getPosition;
    name <- (choice [assignParser, exprStmtParser] `sepBy1` comma) <* semi;
    return $ LineStmt pos name;
} <?> "comma separated line statement"

returnParser :: Parser Stmt
returnParser = do {
    pos <- getPosition;
    reserved "return";
    ReturnStmt pos <$> (exprParser <* semi);
} <?> "return statement"

blockParser :: Parser Stmt
blockParser = do {
    pos <- getPosition;
    reservedOp "{";
    s <- many stmtParser;
    reservedOp "}";
    return $ BlockStmt pos s;
} <?> "compound statement"

ifParser :: Parser Stmt
ifParser = do {
    reserved "if";
    pos <- getPosition;
    cond <- exprParser;
    trBr <- blockParser;
    elseBr <- optionMaybe elseParser;
    return $ IfStmt pos cond trBr elseBr;
}

elseParser :: Parser Stmt
elseParser = do {
    pos <- getPosition;
    reserved "else" >> (try ifParser <|> try blockParser);
}

whileParser :: Parser Stmt
whileParser = do {
    pos <- getPosition;
    ex <- reserved "while" >> exprParser;
    bodyPos <- getPosition;
    WhileStmt pos ex <$> blockParser
} <?> "while statement"

doWhileParser :: Parser Stmt
doWhileParser = do {
    pos <- getPosition;
    body <- reserved "do" >> blockParser;
    ex <- reserved "while" >> exprParser;
    semi;
    return $ DoWhileStmt pos body ex;
} <?> "do-while statement"

switchParser :: Parser Stmt
switchParser = do {
    pos <- getPosition;
    ex <- reserved "switch" >> exprParser;
    SwitchStmt pos ex <$> blockParser;
}

-- The body stmt part is not implemented
caseParser :: Parser Stmt
caseParser = do {
    pos <- getPosition;
    ex <- (reserved "case" >> exprParser) <* reservedOp ":";
    bodyPos <- getPosition;
    CaseStmt pos ex <$> stmtParser;
}

forParser :: Parser Stmt
forParser = do {
    pos <- getPosition;
    reserved "for" >> reservedOp "(";
    varDecPos <- getPosition;
    varDec <- stmtParser;
    endCond <- exprParser <* semi;
    varUpdate <- exprParser;
    reservedOp ")";
    bodyPos <- getPosition;
    ForStmt pos varDec endCond varUpdate <$> blockParser;
} <?> "for statement"

contParser :: Parser Stmt
contParser = do {
    pos <- getPosition;
    reserved "continue" <* semi;
    return (ContStmt pos);
} <?> "continue statement"

brkParser :: Parser Stmt
brkParser = do {
    pos <- getPosition ;
    reserved "break" <* semi;
    return (BrkStmt pos);
} <?> "break statement"

fnDefParser :: Parser Stmt
fnDefParser = do {
    pos <- getPosition;
    tp <- typeParser;
    name <- identParser;
    reservedOp "(";
    try (do {
        reservedOp ")";
        FnDefStmt pos tp name [] <$> blockParser;
    })
    <|> (do {
    para <- exprParser `sepBy` reservedOp ",";
    reservedOp ")";
    FnDefStmt pos tp name para <$> blockParser;
    })
}

-- Not implemented, currently a placeholder
wildCardParser :: Parser Stmt
wildCardParser = do {
    pos <- getPosition;
    wc <- manyTill anyChar (lookAhead eol);
    return (WildCardStmt pos wc)
} <?> "wildcard"
    where eol = try (string "\n\r")
              <|> try (string "\r\n")
              <|> string "\n"
              <|> string "\r"
              <?> "EOL"

exprParser' :: Parser Expr
exprParser' = try varDecParser
    <|> try ptrMemAccParser
    <|> try arrAccParser
    <|> try memAccParser
    <|> try opExprParser
    <|> try fnCallParser
    <|> try atomParser
    <|> try identParser
    <?> "expression"

exprParser :: Parser Expr
exprParser = try (parens exprParser')
    <|> try exprParser'

varDecParser :: Parser Expr
varDecParser = do {
    tp <- typeParser;
    VarDec tp <$> identifier;
}
identParser :: Parser Expr
identParser = Ident <$> identifier

fnCallParser :: Parser Expr
fnCallParser = do {
    name <- identifier;
    reservedOp "(";
    try (do {
        reservedOp ")";
        return $ FnCall name [];
    })
    <|> (do {
    ex <- exprParser `sepBy` reservedOp ",";
    reservedOp ")";
    return $ FnCall name ex;
    }
    )
}

atomParser :: Parser Expr
atomParser = try floatParser
    <|> boolParser
    <|> charParser
    <|> strParser
    <|> intParser
    <?> "constant"

intParser :: Parser Expr
intParser = Atom . IntVal <$> integer
            <?> "integer"

boolParser :: Parser Expr
boolParser = try (reserved "true" >> return (Atom . BoolVal $ True))
    <|> (reserved "false" >> return (Atom . BoolVal $ False))
    <?> "bool"

charParser :: Parser Expr
charParser = do {
    char '\'';
    c <- anyChar;
    char '\'';
    return (Atom . CharVal $ c);
} <?> "char"

strParser :: Parser Expr
strParser = do {
    char '"';
    s <- manyTill anyChar (char '"');
    return (Atom . StrVal $ s);
} <?> "string"


floatParser :: Parser Expr
floatParser = try(Atom . FloatVal <$> float)
    <|> do {
        reservedOp "-";
        n <- float;
        return $ Atom $ FloatVal (-n);
    } <?> "floating point number"

arrAccParser' :: Expr -> Parser Expr
arrAccParser' arr = do {
    acc <- many1 $ brackets exprParser;
    return $ foldl ArrAccess arr acc
}

arrAccParser :: Parser Expr
arrAccParser = do {
    lst <- try fnCallParser
    <|> try identParser;
    arrAccParser' lst;
}

memAccParser' :: Expr -> Parser Expr
memAccParser' obj = do {
    acc <- many1 $ reservedOp "." *> exprParser;
    return $ foldl MemAccess obj acc;
}

memAccParser :: Parser Expr
memAccParser = do {
    obj <- try fnCallParser
    <|> try identParser;
    memAccParser' obj;
}

ptrMemAccParser' :: Expr -> Parser Expr
ptrMemAccParser' obj = do {
    acc <- many1 $ reservedOp "->" *> exprParser;
    return $ foldl PtrMemAccess obj acc;
}

ptrMemAccParser :: Parser Expr
ptrMemAccParser = do {
    obj <- try fnCallParser
    <|> try identParser;
    ptrMemAccParser' obj;
}

opExprParser :: Parser Expr
opExprParser = buildExpressionParser opTable opTerm
    <?> "operator expression"

opTerm :: Parser Expr
opTerm = try fnCallParser
    <|> try atomParser
    <|> try arrAccParser
    <|> try identParser

opTable :: [[Operator Char () Expr]]
opTable = [
        [Prefix (reservedOp "++" >> return (UnaOp PreInc))],
        [Postfix (reservedOp "++" >> return (UnaOp PostInc))],
        [Postfix (reservedOp "--" >> return (UnaOp PostDec))],
        [Prefix (reservedOp "--" >> return (UnaOp PreDec))],
        [Prefix (reservedOp "&" >> return (UnaOp Addr))],
        [Prefix (reservedOp "*" >> return (UnaOp DeRef))],
        [Prefix (reservedOp "!" >> return (UnaOp Not))],
        [Infix (reservedOp ">" >> return (BinOp Gt)) AssocLeft],
        [Infix (reservedOp ">=" >> return (BinOp Gte)) AssocLeft],
        [Infix (reservedOp "<" >> return (BinOp Lt)) AssocLeft],
        [Infix (reservedOp "<=" >> return (BinOp Lte)) AssocLeft],
        [Infix (reservedOp "!=" >> return (BinOp Neq)) AssocLeft],
        [Infix (reservedOp "==" >> return (BinOp Eq)) AssocLeft],
        [Infix (reservedOp "+" >> return (BinOp Add)) AssocLeft],
        [Infix (reservedOp "-" >> return (BinOp Sub)) AssocLeft],
        [Infix (reservedOp "*" >> return (BinOp Mul)) AssocLeft],
        [Infix (reservedOp "/" >> return (BinOp Div)) AssocLeft],
        [Infix (reservedOp ">>" >> return (BinOp RShft)) AssocLeft],
        [Infix (reservedOp "<<" >> return (BinOp LShft)) AssocLeft],
        [Infix (reservedOp "&" >> return (BinOp BitAnd)) AssocLeft],
        [Infix (reservedOp "|" >> return (BinOp BitOr)) AssocLeft],
        [Infix (reservedOp "^" >> return (BinOp BitXor)) AssocLeft],
        [Infix (reservedOp "&&" >> return (BinOp And)) AssocLeft],
        [Infix (reservedOp "||" >> return (BinOp Or)) AssocLeft]
    ]

parseStmt :: String -> Stmt
parseStmt str = case parse stmtParser "" str of
    Left e -> error $ show e
    Right r -> r

parseFile :: String -> IO [Stmt]
parseFile f = do {
    codes <- readFile f;
    case parse (whiteSpace *> manyTill stmtParser eof <* whiteSpace) "" codes of
        Left e -> print e >> fail "failed to parse the source file"
        Right r -> return r
}

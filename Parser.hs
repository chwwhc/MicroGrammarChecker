import MicroGrammar
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token



eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "EOL"

cKeyWords :: [String]
cKeyWords = [
        "auto", "break", "case", "char", "const", "if",
        "continue", "default", "do", "int", "long",
        "register", "return", "short", "signed", "sizeof",
        "static", "struct", "switch", "typedef", "union",
        "unsigned", "void", "volatile", "while", "double",
        "else", "enum", "extern", "float", "for", "goto",
        "else if", "String", "string"
            ]

cKeySymbols :: [String]
cKeySymbols = [
    "*", "=", "||", "&&", "-", "+", "/", "&",
    ",", "(", ")", ">", "<", ">=", "<=", "!=",
    "[", "]", "{", "}", "#", "^", "~", "!", "%",
    "|", ".", "?", "==", ">>", "<<", "'", "\"",
    ":", "->"
        ]

languageDef = javaStyle {
    Token.nestedComments = False,
    Token.reservedNames = cKeyWords,
    Token.reservedOpNames = cKeySymbols,
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
typeParser :: Parser Type
typeParser = (reserved "int" >> return Int)
    <|> (reserved "char" >> return Char)
    <|> (reserved "float" >> return Float)
    <|> (reserved "double" >> return Double)
    <|> (reserved "string" >> return (Pointer Char))
    <?> "C Type"

stmtParser' :: Parser Stmt
stmtParser' = try whileParser
    <|> try ifParser
    <|> try fnDefParser
    <|> try elifParser
    <|> try elseParser
    <|> try switchParser
    <|> try assignParser
    <|> try returnParser
    <|> try forParser
    <|> try blockParser
    <|> try exprStmtParser
    <|> try contParser
    <|> try brkParser
    <|> try doWhileParser
    <?> "statement"

stmtParser :: Parser Stmt
stmtParser = do {
    whiteSpace;
    s <- stmtParser';
    whiteSpace;
    return s;
}

assignParser :: Parser Stmt
assignParser = do {
    pos <- getPosition;
    name <- exprParser;
    reservedOp "=";
    AssignStmt pos name <$> (exprParser <* semi);
}

returnParser :: Parser Stmt
returnParser = do {
    pos <- getPosition;
    reserved "return";
    ReturnStmt pos <$> (exprParser <* semi);
}

blockParser :: Parser Stmt
blockParser = do {
    pos <- getPosition;
    reservedOp "{";
    s <- many $ try stmtParser;
    reservedOp "}";
    return $ BlockStmt pos s;
} <?> "compound statement"

exprStmtParser :: Parser Stmt
exprStmtParser = do {
    pos <- getPosition;
    ExprStmt pos <$> (exprParser <* semi);
} <?> "expression statement"

ifStmtParser' :: String -> Parser Stmt
ifStmtParser' s = do {
    reserved s;
    pos <- getPosition;
    cond <- parens exprParser;
    bodyPos <- getPosition;
    if s == "if" then IfStmt pos cond <$> blockParser
    else ElifStmt pos cond <$> blockParser
}

ifParser :: Parser Stmt
ifParser = ifStmtParser' "if"

elifParser :: Parser Stmt
elifParser = ifStmtParser' "else if"

elseParser :: Parser Stmt
elseParser = do {
    pos <- getPosition;
    body <- reserved "else" >> blockParser;
    return $ ElseStmt pos body;
}

whileParser :: Parser Stmt
whileParser = do {
    pos <- getPosition;
    ex <- reserved "while" >> parens exprParser;
    bodyPos <- getPosition;
    WhileStmt pos ex <$> blockParser
} <?> "while statement"

doWhileParser :: Parser Stmt
doWhileParser = do {
    pos <- getPosition;
    body <- reserved "do" >> blockParser;
    ex <- reserved "while" >> parens exprParser;
    semi;
    return $ DoWhileStmt pos body ex;
} <?> "do-while statement"

switchParser :: Parser Stmt
switchParser = do {
    pos <- getPosition;
    ex <- reserved "switch" >> parens exprParser;
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
    ForStmt pos varDec endCond varUpdate <$> stmtParser;
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
    para <- exprParser `sepBy` reservedOp ",";
    reservedOp ")";
    FnDefStmt pos tp name para <$> stmtParser;
}

exprParser :: Parser Expr
exprParser = try atomParser
    <|> try varDecParser
    <|> try fnCallParser
    <|> try opExprParser
    <|> try identParser
    <|> try arrAccParser
    <|> try memAccParser
    <|> try ptrMemAccParser
    <|> try wildCardParser
    <?> "expression"

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
    ex <- exprParser `sepBy` reservedOp ",";
    reservedOp ")";
    return $ FnCall name ex
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
    return (Atom . ListVal $ map CharVal s);
} <?> "string"


floatParser :: Parser Expr
floatParser = try(Atom . FloatVal <$> float)
    <|> do {
        reservedOp "-";
        n <- float;
        return $ Atom $ FloatVal (-n);
    } <?> "floating point number"

arrAccParser :: Parser Expr
arrAccParser = do {
    lst <- exprParser;
    idx <- brackets exprParser;
    return $ ArrAccess lst idx
}

memAccParser :: Parser Expr
memAccParser = do {
    cls <- exprParser <* reservedOp ".";
    MemAccess cls <$> exprParser;
}

ptrMemAccParser :: Parser Expr
ptrMemAccParser = do {
    cls <- exprParser <* reservedOp "->";
    PtrMemAccess cls <$> exprParser;
}

-- Not implemented, currently a placeholder
wildCardParser :: Parser Expr
wildCardParser = do {
    pos <- getPosition;
    return (WildCard [(pos, "wildCardParser PlaceHolder")])
} <?> "wildcard"

opExprParser :: Parser Expr
opExprParser = buildExpressionParser opTable opTerm
    <?> "operator expression"

opTerm :: Parser Expr
opTerm = parens opExprParser
    <|> (reserved "true" >> return (Atom $ BoolVal True))
    <|> (reserved "false" >> return (Atom $ BoolVal False))
    <|> Atom . IntVal <$> integer
    <|> Ident <$> identifier

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
    Left lft -> error $ show lft
    Right rght -> rght

parseFile :: String -> IO [Stmt]
parseFile f = do {
    codes <- readFile f;
    case parse (stmtParser `endBy1` eol)"" codes of
        Left e -> print e >> fail "failed to parse the source file"
        Right r -> return r
}
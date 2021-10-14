import MicroGrammar
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

cKeyWords :: [String]
cKeyWords = [
        "auto", "break", "case", "char", "const", "if",
        "continue", "default", "do", "int", "long",
        "register", "return", "short", "signed", "sizeof",
        "static", "struct", "switch", "typedef", "union",
        "unsigned", "void", "volatile", "while", "double",
        "else", "enum", "extern", "float", "for", "goto"
            ]

cKeySymbols :: [String]
cKeySymbols = [
    "*", "=", "||", "&&", "-", "+", "/", "&",
    ",", "(", ")", ">", "<", ">=", "<=", "!=",
    "[", "]", "{", "}", "#", "^", "~", "!", "%",
    "|", ".", "?", "==", ">>", "<<", "'", "\""
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

balancedPParser :: Parser ()
balancedPParser = choice [between (char l) (char r) balancedPParser |
    (l, r) <- [('(', ')'), ('[', ']'), ('{', '}')]]

stmtParser :: Parser Stmt
stmtParser = try whileParser
    <|> try ifParser
    <|> try assignParser
    <|> try returnParser
    <|> try forParser
    <|> try cpdStmtParser
    <|> try exprStmtParser
    <?> "statement"

exprParser :: Parser Expr
exprParser = try binExprParser
    <|> try unaExprParser
    <|> try varDecParser
    <|> try constParser
    <|> try identParser
    <|> try wildCardParser
    <?> "expression"

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


-- Not complete
cTypeParser :: Parser CType
cTypeParser = (reserved "int" >> return CInt)
    <|> (reserved "char" >> return CChar)
    <?> "C Type"

-- Should be OK
varDecParser :: Parser Expr
varDecParser = do {
    tp <- cTypeParser;
    VarDec tp <$> identifier;
}

identParser :: Parser Expr
identParser = Ident <$> identifier

constParser :: Parser Expr
constParser = try intParser
    <|> boolParser
    <|> charParser
    <|> strParser
    <?> "constant"

intParser :: Parser Expr
intParser = Const . CIntVal <$> integer
            <?> "integer"

boolParser :: Parser Expr
boolParser = try (reserved "true" >> return (Const . CBoolVal $ True))
    <|> (reserved "false" >> return (Const . CBoolVal $ False))
    <?> "bool"

charParser :: Parser Expr
charParser = do {
    char '\'';
    c <- anyChar;
    char '\'';
    return (Const . CCharVal $ c);
} <?> "char"

strParser :: Parser Expr
strParser = do {
    char '\"';
    s <- manyTill anyChar (char '"');
    return (Const . CStringVal $ s);
} <?> "string"

-- Not implemented, currently a placeholder
wildCardParser :: Parser Expr
wildCardParser = do {
    pos <- getPosition;
    return (WildCard [(pos, "wildCardParser PlaceHolder")])
} <?> "wildcard"

cpdStmtParser :: Parser Stmt
cpdStmtParser = do {
    pos <- getPosition;
    reservedOp "{";
    s <- many $ try stmtParser;
    return $ CompoundStmt pos s;
} <?> "compound statement"

exprStmtParser :: Parser Stmt
exprStmtParser = do {
    pos <- getPosition;
    ExprStmt pos <$> (exprParser <* semi);
} <?> "expression statement"

-- Not complete
ifParser :: Parser Stmt
ifParser = do {
    reserved "if";
    pos <- getPosition;
    cond <- parens exprParser;
    trPos <- getPosition;
    reserved "else";
    elPos <- getPosition;
    return (IfStmt pos cond (CompoundStmt trPos [NoneStmt]) (CompoundStmt elPos [NoneStmt]));
} <?> "if statement"

-- Not complete
whileParser :: Parser Stmt
whileParser = do {
    pos <- getPosition;
    ex <- reserved "while" >> parens exprParser;
    bodyPos <- getPosition;
    WhileStmt pos ex <$> cpdStmtParser
} <?> "while statement"

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

unaExprParser :: Parser Expr
unaExprParser = buildExpressionParser unaTable unaTerm
    <?> "unary expression"

binExprParser :: Parser Expr
binExprParser = buildExpressionParser binTable binTerm
    <?> "binary expression"

unaTerm :: Parser Expr
unaTerm = parens unaExprParser
    <|> Ident <$> identifier
    <|> Const . CIntVal <$> integer
    <?> "simple expresion"

binTerm :: Parser Expr
binTerm = parens binExprParser
    <|> (reserved "true" >> return (Const $ CBoolVal True))
    <|> (reserved "false" >> return (Const $ CBoolVal False))
    <|> Const . CIntVal <$> integer
    <|> Ident <$> identifier

unaTable :: [[Operator Char () Expr]]
unaTable = [
        [Prefix (reservedOp "++" >> return (UnaOp PreInc))],
        [Prefix (reservedOp "--" >> return (UnaOp PreDec))],
        [Prefix (reservedOp "&" >> return (UnaOp Addr))],
        [Prefix (reservedOp "*" >> return (UnaOp DeRef))],
        [Prefix (reservedOp "!" >> return (UnaOp Not))]
    ]

binTable :: [[Operator Char () Expr]]
binTable = [
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


parseString :: String -> Stmt
parseString str = case parse stmtParser "" str of
    Left lft -> error $ show lft
    Right rght -> rght

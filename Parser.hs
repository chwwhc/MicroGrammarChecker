import MicroGrammar
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

cKeyWords = [
        "if", "else if", "else", "for", "while",
        "int", "float", "double", "char", "void",
        "true", "false"
            ]

cKeySymbols = [
    "*", "=", "||", "&&", "-", "+", "/", "&",
    ",", "(", ")", ">", "<", ">=", "<=", "!="
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
    <|> try assignParser
    <|> try forParser
    <|> try ifParser
    <|> try exprStmtParser

exprParser :: Parser Expr
exprParser = try binExprParser
    <|> try unaExprParser
    <|> try identParser
    <|> try varDecParser
    <|> try wildCardParser

assignParser :: Parser Stmt
assignParser = do {
    pos <- getPosition;
    name <- exprParser;
    reservedOp "=";
    AssignStmt pos name <$> exprParser;
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
identParser = do {
    Ident <$> identifier;
}

-- Should be OK
intParser :: Parser Expr
intParser = Const . CIntVal <$> integer

-- Not implemented, currently a placeholder
wildCardParser :: Parser Expr
wildCardParser = do {
    pos <- getPosition;
    return (WildCard [(pos, "wildCardParser PlaceHolder")])
}

exprStmtParser :: Parser Stmt
exprStmtParser = do {
    pos <- getPosition;
    ExprStmt pos <$> exprParser;
}

-- Not complete
ifParser :: Parser Stmt
ifParser = do {
    reserved "if";
    pos <- getPosition;
    cond <- parens exprParser;
    trBr <- braces exprParser;
    reserved "else";
    elBr <- braces exprParser;
    return (IfStmt pos cond trBr elBr);
}

-- Not complete
whileParser :: Parser Stmt
whileParser = do {
    pos <- getPosition;
    ex <- reserved "while" >> parens exprParser;
    st <- braces exprParser;
    return (WhileStmt pos ex st)
}

forParser :: Parser Stmt
forParser = do {
    pos <- getPosition;
    reserved "for" >> reservedOp "(";
    varDecPos <- getPosition;
    varDec <- stmtParser <* semi;
    endCond <- exprParser <* semi;
    varUpdate <- exprParser;
    reservedOp ")";
    bodyPos <- getPosition;
    body <- wildCardParser;
    return (ForStmt pos varDec endCond varUpdate (CompoundStmt bodyPos [NoneStmt]));
}

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

unaTable = [
        [Prefix (reservedOp "++" >> return (UnaOp PreInc))],
        [Prefix (reservedOp "&" >> return (UnaOp Addr))],
        [Prefix (reservedOp "*" >> return (UnaOp DeRef))]
    ]

binTable = [
    [Infix (reservedOp ">" >> return (BinOp Gt)) AssocLeft],
    [Infix (reservedOp ">=" >> return (BinOp Gte)) AssocLeft],
    [Infix (reservedOp "<" >> return (BinOp Lt)) AssocLeft],
    [Infix (reservedOp "<=" >> return (BinOp Lte)) AssocLeft]
    ]


parseString :: String -> Stmt
parseString str = case parse stmtParser "" str of
    Left lft -> error $ show lft
    Right rght -> rght

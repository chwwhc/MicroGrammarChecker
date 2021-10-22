module Parser where
import MicroGrammar
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

{-- 
todo： 
wildcard parser
switch case parser
variable declaration parser 
array initialization parser 
struct parser 
union parser 
class parser
--}


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
    ":", "->", "[]", "+=", "-=", "*=", "/=",
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

stmtParser :: Parser Stmt
stmtParser = whileParser
    <|> ifParser
    <|> try fnDefParser
    <|> switchParser
    <|> returnParser
    <|> forParser
    <|> contParser
    <|> brkParser
    <|> doWhileParser
    <|> try lnParser
    <|> try blockParser
    <|> try wcStmtParser
    <?> "statement"

lnParser :: Parser Stmt
lnParser = do {
    ExprStmt <$> exprParser <* (try (semi *> whiteSpace) <|> whiteSpace);
} <?> "comma separated line statement"

wcStmtParser :: Parser Stmt
wcStmtParser = ExprStmt <$> exprParser

returnParser :: Parser Stmt
returnParser = do {
    reserved "return" >> ReturnStmt <$> (exprParser <* semi);
} <?> "return statement"

blockParser :: Parser Stmt
blockParser = do {
    BlockStmt <$> braces (many stmtParser);
} <?> "compound statement"

ifParser :: Parser Stmt
ifParser = do {
    reserved "if";
    cond <- exprParser;
    trBr <- blockParser;
    IfStmt cond trBr <$> optionMaybe elseParser;
}

elseParser :: Parser Stmt
elseParser = do {
    pos <- getPosition;
    reserved "else" >> (ifParser <|> blockParser);
}

whileParser :: Parser Stmt
whileParser = do {
    ex <- reserved "while" >> exprParser;
    bodyPos <- getPosition;
    WhileStmt ex <$> blockParser;
} <?> "while statement"

doWhileParser :: Parser Stmt
doWhileParser = do {
    body <- reserved "do" >> blockParser;
    DoWhileStmt body <$> (reserved "while" >> exprParser <* semi);
} <?> "do-while statement"

switchParser :: Parser Stmt
switchParser = do {
    ex <- reserved "switch" >> exprParser;
    SwitchStmt ex <$> blockParser;
}

-- The body stmt part is not implemented
caseParser :: Parser Stmt
caseParser = do {
    ex <- (reserved "case" >> exprParser) <* symbol ":";
    bodyPos <- getPosition;
    CaseStmt ex <$> stmtParser;
}

forParser :: Parser Stmt
forParser = do {
    reserved "for" >> symbol "(";
    varDecPos <- getPosition;
    varDec <- stmtParser;
    endCond <- exprParser <* semi;
    varUpdate <- exprParser;
    symbol ")";
    bodyPos <- getPosition;
    ForStmt varDec endCond varUpdate <$> blockParser;
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

fnDefParser :: Parser Stmt
fnDefParser = do {
    tp <- typeParser;
    name <- identParser;
    symbol "(";
    try (do {
        symbol ")";
        FnDefStmt tp name [] <$> blockParser;
    })
    <|> (do {
    para <- (typeParser *> identParser <* skipMany (oneOf "[]")) `sepBy` comma;
    symbol ")";
    FnDefStmt tp name para <$> blockParser;
    })
}

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
exprParser = try arrDecParser
    <|> try varDecParser
    <|> buildExpressionParser opTable term
    <?> "operator expression"

-- not correct
varDecParser :: Parser Expr
varDecParser = do {
    tp <- typeParser;
    VarDec tp <$> (exprParser `sepBy` comma);
}

arrDecParser' :: Parser Expr
arrDecParser' = do {
    buildExpressionParser [[postfixChain arrInit]] (identParser <* lookAhead (symbol "["));
}
    where arrInit = do {
    pos <- getPosition;
    idx <- brackets atomParser;
    return $ \lst -> ArrInit pos (lst, idx);
}

-- not correct
arrDecParser :: Parser Expr
arrDecParser = do {
    tp <- typeParser;
    ArrDec tp <$> (arrDecParser' `sepBy` comma)
}

{-- 
wildCardParser :: Parser Expr
wildCardParser = do {
    pos <- getPosition;
    wc <- whiteSpace *> many (noneOf " \t\n\r\f\v") <* whiteSpace;
    return $ WildCard wc;
}
--}

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
    <|> identParser
    <?> "constant"

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
    -- <|> wildCardParser

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


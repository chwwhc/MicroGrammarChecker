import System.IO
import MicroGrammar
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef = javaStyle {
    Token.nestedComments = False,
    Token.reservedNames = ["if", "else if", "else"],
    Token.reservedOpNames = ["*", "=", "||", "&&"],
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
natural :: Parser Integer
natural = Token.natural lexer
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

intParser :: Parser Expr
intParser = do {
    pos <- getPosition;
    Const pos . CIntVal <$> natural;
}

balancedPParser :: Parser ()
balancedPParser = choice [between (char l) (char r) balancedPParser |
    (l, r) <- [('(', ')'), ('[', ']'), ('{', '}')]]


exprParser :: Parser Expr
exprParser = try intParser
   -- <|> try wildCardParser

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

parseString :: String -> Stmt
parseString str = case parse ifParser "" str of
    Left lft -> error $ show lft
    Right rght -> rght


module MicroGrammar where 

import Text.Parsec

type Identifier = String 
type UnknownTok = (SourcePos, String)

data Stmt = IfStmt SourcePos Expr Stmt
    | ElifStmt SourcePos Expr Stmt 
    | ElseStmt SourcePos Stmt 
    | WhileStmt SourcePos Expr Stmt
    | DoWhileStmt SourcePos Stmt Expr 
    | ForStmt SourcePos Stmt Expr Expr Stmt 
    | ContStmt SourcePos 
    | BrkStmt SourcePos 
    | SwitchStmt SourcePos Expr Stmt 
    | CaseStmt SourcePos Expr Stmt
    | FnDefStmt SourcePos Type Expr [Expr] Stmt
    | ReturnStmt SourcePos Expr 
    | ExprStmt SourcePos Expr 
    | AssignStmt SourcePos Expr Expr 
    | BlockStmt SourcePos [Stmt]
    | NoneStmt 
    deriving (Show)


data Expr = WildCard [UnknownTok]
    | TernOp Expr Expr Expr 
    | BinOp BinOpSym Expr Expr 
    | UnaOp UnOpSym Expr
    | VarDec Type Identifier
    | Ident Identifier 
    | FnCall Identifier [Expr]
    | ArrAccess Expr Expr 
    | MemAccess Expr Expr 
    | PtrMemAccess Expr Expr
    | Atom Val
    | VoidExpr
    | NoneExpr
    deriving (Show)

data Type = Int
    | Bool
    | Float 
    | Double
    | Char
    | Void 
    | Pointer Type
    | Struct Identifier 
    | Union Identifier  
    | NoneType
    deriving (Show)

data Val = IntVal Integer 
    | FloatVal Double
    | BoolVal Bool
    | CharVal Char 
    | ListVal [Val]
    | VoidVal 
    | NoneVal
    deriving (Show)

data UnOpSym = Not 
    | PreInc 
    | PostInc
    | PreDec 
    | PostDec
    | Addr 
    | DeRef
    | BitNeg 
    | Cast Type 
    | SizeOf 
    deriving (Show)

data BinOpSym = Add 
    | Sub 
    | Mul 
    | Div 
    | Mod 
    | LShft 
    | RShft
    | Lt 
    | Lte 
    | Gt 
    | Gte 
    | Eq 
    | Neq 
    | BitAnd 
    | BitOr 
    | BitXor 
    | And 
    | Or 
    deriving (Show)


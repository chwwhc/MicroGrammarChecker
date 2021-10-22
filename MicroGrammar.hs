module MicroGrammar where 

import Text.Parsec

type Identifier = String 
type UnknownTok = (SourcePos, String)

data Stmt = IfStmt Expr Stmt (Maybe Stmt)
    | ElseStmt Stmt 
    | WhileStmt Expr Stmt
    | DoWhileStmt Stmt Expr 
    | ForStmt Stmt Expr Expr Stmt 
    | ContStmt 
    | BrkStmt 
    | SwitchStmt Expr Stmt 
    | CaseStmt Expr Stmt
    | FnDefStmt Type Expr [Expr] Stmt
    | ReturnStmt Expr 
    | ExprStmt Expr 
    | BlockStmt [Stmt]
    | VoidStmt 
    deriving (Show)

data Expr = WildCard String
    | TernOp TerOpSym Expr Expr Expr 
    | BinOp BinOpSym Expr Expr 
    | UnaOp UnOpSym Expr
    | PtrMemAcc SourcePos Expr Expr
    | MemAcc SourcePos Expr Expr
    | ArrAcc SourcePos Expr Expr
    | Ident SourcePos Identifier 
    | FnCall SourcePos Identifier [Expr]
    | Atom SourcePos Val
    | Assign BinOpSym Expr Expr
    | VarDec Type [Expr]
    | ArrInit SourcePos (Expr, Expr)
    | ArrDec Type [Expr]
    | VoidExpr SourcePos
    deriving (Show)

data Type = Int
    | Bool
    | Float 
    | Double
    | Char
    | Void 
    | Pointer Type
    | Reference Type
    | Array Type
    | Struct Identifier 
    | Union Identifier  
    | Const Type
    deriving (Show)

data Val = IntVal Integer 
    | FloatVal Double
    | BoolVal Bool
    | CharVal Char 
    | StrVal String
    | VoidVal 
    deriving (Show)

data UnOpSym = Not 
    | PreInc 
    | PostInc
    | PreDec 
    | PostDec
    | Addr 
    | DeRef
    | BitNeg 
    | Pos 
    | Minus
    | SizeOf 
    | TypeCast Type
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
    | NoOpAssign
    | AddAssign
    | SubAssign
    | MulAssign
    | DivAssign
    | ModAssign 
    | BitAndAssign
    | BitOrAssign
    | BitXorAssign
    | RshftAssign
    | LshftAssign
    deriving (Show)

data TerOpSym = TerIfElse 
    deriving (Show)


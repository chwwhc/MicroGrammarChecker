module AST where 

import Text.Parsec

type Identifier = String 

data Stmt = IfStmt SourcePos Expr Stmt (Maybe Stmt)
    | WhileStmt SourcePos Expr Stmt
    | DoWhileStmt SourcePos Stmt Expr 
    | ForStmt SourcePos Expr Expr Expr Stmt 
    | ContStmt 
    | BrkStmt 
    | SwitchStmt SourcePos Expr Stmt 
    | CaseStmt SourcePos Expr Stmt
    | DefaultStmt SourcePos Stmt
    | FnDecStmt SourcePos Identifier [Expr] Stmt
    | ReturnStmt Expr 
    | LineStmt [Expr]
    | BodyStmt [Stmt]
    | GotoStmt SourcePos Identifier
    | OtherStmt
    deriving (Show)

data Expr = WildCard [String]
    | TernOp TerOpSym Expr Expr Expr 
    | BinOp BinOpSym Expr Expr 
    | UnaOp UnOpSym Expr
    | PtrMemAcc SourcePos Expr Expr
    | MemAcc SourcePos Expr Expr
    | ArrAcc SourcePos Expr Expr
    | Ident SourcePos Identifier 
    | Call SourcePos Identifier [Expr]
    | Atom SourcePos Val
    | Assign BinOpSym Expr Expr 
    | VarDec (Type, Expr)
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
    | Struct Identifier 
    | Union Identifier  
    | Const Type
    | UnknownType Identifier
    deriving (Show)

data Val = IntVal Integer 
    | FloatVal Double
    | BoolVal Bool
    | CharVal Char 
    | StrVal String
    | ListVal [Expr]
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


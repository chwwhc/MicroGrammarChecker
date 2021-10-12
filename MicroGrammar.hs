module MicroGrammar where 

import Text.Parsec

type Identifier = String 

data Stmt = IfStmt SourcePos Expr Expr Expr
    | WhileStmt SourcePos Expr Stmt 
    | DoWhileStmt SourcePos Stmt Expr 
    | ForStmt SourcePos Expr Expr Expr Stmt 
    | ContStmt 
    | BrkStmt 
    | SwitchStmt SourcePos Expr Stmt 
    | CaseStmt SourcePos Expr 
    | ReturnStmt SourcePos Expr 
    | ExprStmt SourcePos Expr 
    | CompoundStmt SourcePos [Stmt]
    | Line SourcePos Stmt
    | NoneStmt 
    deriving (Show)


data Expr = WildCard SourcePos [String]
    | TernOp SourcePos Expr Expr Expr 
    | BinOp SourcePos BinOpSym Expr Expr 
    | UnOp SourcePos UnOpSym Expr
    | Assign SourcePos Expr Expr 
    | VarDec SourcePos CType Identifier
    | Ident SourcePos Identifier 
    | FnCall SourcePos Identifier [Expr]
    | ArrAccess SourcePos Expr Expr 
    | MemAccess SourcePos Expr Expr 
    | PtrAccess SourcePos Expr Expr
    | Var SourcePos Identifier 
    | Const SourcePos CVal
    | NoneExpr
    deriving (Show)

data CType = CInt 
    | CFloat 
    | CDouble 
    | CChar 
    | CVoid 
    | CPointer CType
    | CStruct Identifier  
    | NoneType
    deriving (Show)

data CVal = CIntVal Integer 
    | CFloatVal Float 
    | CDoubleVal Double 
    | CCharVal Char 
    | CVoidVal 
    | CStringVal String 
    deriving (Show)

data UnOpSym = Minus
    | Plus
    | Not 
    | PreInc 
    | PreDec 
    | Addr 
    | Indirec 
    | BitNeg 
    | Cast CType 
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

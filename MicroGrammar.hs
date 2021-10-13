module MicroGrammar where 

import Text.Parsec

type Identifier = String 
type UnknownTok = (SourcePos, String)

data Stmt = IfStmt SourcePos Expr Expr Expr
    | WhileStmt SourcePos Expr Expr
    | DoWhileStmt SourcePos Stmt Expr 
    | ForStmt SourcePos Stmt Expr Expr Stmt 
    | ContStmt SourcePos 
    | BrkStmt SourcePos 
    | SwitchStmt SourcePos Expr Stmt 
    | CaseStmt SourcePos Expr 
    | ReturnStmt SourcePos Expr 
    | ExprStmt SourcePos Expr 
    | AssignStmt SourcePos Expr Expr 
    | CompoundStmt SourcePos [Stmt]
    | Line SourcePos Stmt
    | NoneStmt 
    deriving (Show)


data Expr = WildCard [UnknownTok]
    | TernOp Expr Expr Expr 
    | BinOp BinOpSym Expr Expr 
    | UnaOp UnOpSym Expr
    | VarDec CType Identifier
    | Ident Identifier 
    | FnCall Identifier [Expr]
    | ArrAccess Expr Expr 
    | MemAccess Expr Expr 
    | PtrAccess Expr Expr
    | Const CVal
    | ExprLst [Expr]
    | NoneExpr
    deriving (Show)

data CType = CInt 
    | CBool 
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
    | CBoolVal Bool
    | CDoubleVal Double 
    | CCharVal Char 
    | CStringVal String 
    | NoneVal
    deriving (Show)

data UnOpSym = Not 
    | PreInc 
    | PreDec 
    | Addr 
    | DeRef
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

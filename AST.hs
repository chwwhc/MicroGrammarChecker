module AST where
import Text.Parsec ( SourcePos )



type Identifier = String

data Stmt = IfStmt SourcePos Expr Stmt (Maybe Stmt)
    | WhileStmt SourcePos Expr Stmt
    | DoWhileStmt SourcePos Stmt Expr
    | ForStmt SourcePos Expr Expr Expr Stmt
    | CntStmt SourcePos
    | BrkStmt SourcePos 
    | IterStmt SourcePos Expr Expr Stmt
    | TryStmt SourcePos Stmt
    | CatchStmt SourcePos Expr Stmt 
    | FinalStmt SourcePos Stmt
    | LabelStmt SourcePos Identifier
    | SwitchStmt SourcePos Expr Stmt
    | CaseStmt SourcePos Expr Stmt
    | DfltStmt SourcePos Stmt
    | FnDecStmt SourcePos Identifier [Expr] Stmt
    | ReturnStmt SourcePos Expr
    | LineStmt SourcePos [Expr]
    | GotoStmt SourcePos Identifier
    | ClassStmt Identifier Stmt
    | OtherStmt
    | BodyStmt [Stmt]
    deriving (Show)
instance Eq Stmt where
    x == y = case x of
        ClassStmt {} -> False 
        IterStmt _ dec1 name1 body1 -> case y of 
            IterStmt _ dec2 name2 body2 -> dec1 == dec2 && name1 == name2 && body1 == body2 
            _ -> False 
        TryStmt _ body1 -> case y of 
            TryStmt _ body2 -> body1 == body2 
            _ -> False 
        CatchStmt _ ex1 body1 -> case y of 
            CatchStmt _ ex2 body2 -> ex1 == ex2 && body1 == body2 
            _ -> False
        FinalStmt _ body1 -> case y of 
            FinalStmt _ body2 -> body1 == body2 
            _ -> False
        IfStmt _ ex1 _ _ -> case y of
            IfStmt _ ex2 _ _ -> ex1 == ex2
            _ -> False
        WhileStmt _ ex1 body1 -> case y of
            WhileStmt _ ex2 body2 -> ex1 == ex2 && body1 == body2
            _ -> False
        DoWhileStmt _ body1 ex1 -> case y of
            DoWhileStmt _ body2 ex2 -> ex1 == ex2 && body1 == body2
            _ -> False
        ForStmt _ dec1 end1 up1 body1 -> case y of
            ForStmt _ dec2 end2 up2 body2 -> dec1 == dec2 && end1 == end2 && up1 == up2 && body1 == body2
            _ -> False
        CntStmt _ -> case y of
            CntStmt _ -> True
            _ -> False
        BrkStmt _ -> case y of
            BrkStmt _ -> True
            _ -> False
        LabelStmt _ lab1 -> case y of
            LabelStmt _ lab2 -> lab1 == lab2
            _ -> False
        SwitchStmt _ ex1 body1 -> case y of
            SwitchStmt _ ex2 body2 -> ex1 == ex2 && body1 == body2
            _ -> False
        CaseStmt _ ex1 body1 -> case y of
            CaseStmt _ ex2 body2 -> ex1 == ex2
            _ -> False
        DfltStmt _ body1 -> case y of
            DfltStmt _ body2 -> body1 == body2
            _ -> False
        FnDecStmt _ name1 params1 body1 -> case y of
            FnDecStmt _ name2 params2 body2 -> name1 == name2 && params1 == params2
            _ -> False
        ReturnStmt _ ex1 -> case y of
            ReturnStmt _ ex2 -> ex1 == ex2
            _ -> False
        LineStmt _ exs1 -> case y of
            LineStmt _ exs2 -> exs1 == exs2
            _ -> False
        BodyStmt stmts1 -> case y of
            BodyStmt stmts2 -> stmts1 == stmts2
            _ -> False
        GotoStmt _ lab1 -> case y of
            GotoStmt _ lab2 -> lab1 == lab2
            _ -> False
        OtherStmt -> case y of
            OtherStmt -> True
            _ -> False

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
    | SizeOf Type
    | VoidExpr
    deriving (Show, Ord)
instance Eq Expr where
    x == y = case x of
        TernOp sym1 cond1 trBr1 elBr1 -> case y of
            TernOp sym2 cond2 trBr2 elBr2 -> sym1 == sym2 && cond1 == cond2 && trBr1 == trBr2 && elBr1 == elBr2
            _ -> False
        BinOp sym1 l1 r1 -> case y of
            BinOp sym2 l2 r2 -> sym1 == sym2 && l1 == l2 && r1 == r2
            _ -> False
        UnaOp sym1 ex1 -> case y of
            UnaOp sym2 ex2 -> sym1 == sym2 && ex1 == ex2
            _ -> False
        PtrMemAcc _ obj1 attr1 -> case y of
            PtrMemAcc _ obj2 attr2 -> obj1 == obj2 && attr1 == attr2
            _ -> False
        MemAcc _ obj1 attr1 -> case y of
            MemAcc _ obj2 attr2 -> obj1 == obj2 && attr1 == attr2
            _ -> False
        ArrAcc _ arr1 idx1 -> case y of
            ArrAcc _ arr2 idx2 -> arr1 == arr2 && idx1 == idx2
            _ -> False
        Ident _ name1 -> case y of
            Ident _ name2 -> name1 == name2
            _ -> False
        Call _ name1 params1 -> case y of
            Call _ name2 params2 -> name1 == name2 && params1 == params2
            _ -> False
        Atom _ val1 -> case y of
            Atom _ val2 -> val1 == val2
            _ -> False
        Assign _ var1 val1 -> case y of
            Assign _ var2 val2 -> var1 == var2 && val1 == val2
            _ -> False
        VarDec (tp1, var1) -> case y of
            VarDec (tp2, var2) -> tp1 == tp2 && var1 == var2
            _ -> False
        SizeOf tp1 -> case y of
            SizeOf tp2 -> tp1 == tp2
            _ -> False
        VoidExpr -> case y of
            VoidExpr -> True
            _ -> False
        _ -> False

data Type = Int
    | UnsignedInt
    | Long
    | LongLong
    | UnsignedLong
    | UnsignedLongLong
    | Short
    | UnsignedShort
    | Bool
    | Float
    | Double
    | LongDouble
    | Char
    | UnsignedChar
    | Void
    | Auto
    | Pointer Type
    | Reference Type
    | Struct Identifier
    | Union Identifier
    | Enum Identifier
    | Const Type
    | Array Type
    | UnknownType Identifier
    deriving (Show, Eq, Ord)

data Val = IntVal Integer
    | FloatVal Double
    | BoolVal Bool
    | CharVal Char
    | StrVal String
    | ListVal [Expr]
    | NullVal
    deriving (Show, Eq, Ord)

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
    | TypeCast Type
    deriving (Show, Eq, Ord)

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
    deriving (Show, Eq, Ord)

data TerOpSym = TerIfElse
    deriving (Show, Eq, Ord)
import CParser
import AST
import Data.List ( (\\), intercalate, intersect, nub )
import Control.Monad ( liftM2 )



findDeref :: [Expr] -> [Expr] -> [Expr]
findDeref exLst acc = case exLst of
            [] -> acc
            (x:xs) -> case x of
                UnaOp DeRef ex -> case ex of
                    Ident {} -> findDeref xs (acc ++ [ex])
                    _ -> findDeref xs acc
                PtrMemAcc _ objOut memOut -> case objOut of
                    PtrMemAcc _ objIn memIn -> findDeref [objOut] (acc ++ [memIn])
                    Ident {} -> findDeref xs (acc ++ [objOut])
                    _ -> findDeref xs acc
                BinOp _ l r -> findDeref xs (acc ++ findDeref [l] [] ++ findDeref [r] [])
                UnaOp _ ex -> findDeref xs (acc ++ findDeref [ex] [])
                MemAcc _ obj mem -> findDeref xs (acc ++ findDeref [obj] [] ++ findDeref [mem] [])
                ArrAcc _ arr idx -> findDeref xs (acc ++ findDeref [arr] [] ++ findDeref [idx] [])
                Call _ _ exLst -> findDeref xs (acc ++ findDeref exLst [])
                Assign _ _ val -> findDeref xs (acc ++ findDeref [val] [])
                VarDec (_, ex) -> findDeref xs (acc ++ findDeref [ex] [])
                _ -> findDeref xs acc

isIdent :: Expr -> Maybe Expr
isIdent ex = case ex of
    UnaOp DeRef var -> Just var
    Ident {} -> Just ex
    _ -> Nothing

findAssign :: [Expr] -> [Expr] -> [Expr]
findAssign exLst acc = case exLst of
    [] -> acc
    (x:xs) -> case x of
        Assign NoOpAssign var _ -> case isIdent var of
            Just ex -> findAssign xs (acc ++ [ex])
            Nothing -> findAssign xs acc
        Assign AddAssign var _ -> case isIdent var of
            Just ex -> findAssign xs (acc ++ [ex])
            Nothing -> findAssign xs acc
        Assign SubAssign var _ -> case isIdent var of
            Just ex -> findAssign xs (acc ++ [ex])
            Nothing -> findAssign xs acc
        Assign MulAssign var _ -> case isIdent var of
            Just ex -> findAssign xs (acc ++ [ex])
            Nothing -> findAssign xs acc
        Assign DivAssign var _ -> case isIdent var of
            Just ex -> findAssign xs (acc ++ [ex])
            Nothing -> findAssign xs acc
        Assign ModAssign var _ -> case isIdent var of
            Just ex -> findAssign xs (acc ++ [ex])
            Nothing -> findAssign xs acc
        Assign BitAndAssign var _ -> case isIdent var of
            Just ex -> findAssign xs (acc ++ [ex])
            Nothing -> findAssign xs acc
        Assign BitOrAssign var _ -> case isIdent var of
            Just ex -> findAssign xs (acc ++ [ex])
            Nothing -> findAssign xs acc
        Assign BitXorAssign var _ -> case isIdent var of
            Just ex -> findAssign xs (acc ++ [ex])
            Nothing -> findAssign xs acc
        Assign RshftAssign var _ -> case isIdent var of
            Just ex -> findAssign xs (acc ++ [ex])
            Nothing -> findAssign xs acc
        Assign LshftAssign var _ -> case isIdent var of
            Just ex -> findAssign xs (acc ++ [ex])
            Nothing -> findAssign xs acc
        VarDec (_, ex) -> findAssign xs (acc ++ findAssign [ex] [])
        _ -> findAssign xs acc

extractIdent :: [Expr] -> [Expr] -> [Expr]
extractIdent exLst acc = case exLst of
    [] -> acc
    (x:xs) -> case x of
        BinOp _ l r -> case (l, r) of
            (Atom _ NullVal, _) -> extractIdent xs acc
            (_, Atom _ NullVal) -> extractIdent xs acc
            _ ->  extractIdent xs (acc ++ extractIdent [l] [] ++ extractIdent [r] [])
        Ident _ var -> extractIdent xs (acc ++ [x])
        _ -> extractIdent xs acc

checkNullPtr :: [String] -> [Expr] -> [Stmt] -> [String]
checkNullPtr acc notNull stmts = case stmts of
    [] -> acc
    (x:xs) -> case x of
        IfStmt _ cond trBr (Just elBr) -> case notNull `intersect` extractIdent [cond] [] of
            [] -> checkNullPtr (acc ++ checkNullPtr [] notNull [trBr] ++ checkNullPtr [] notNull [elBr]) notNull xs
            (y:ys) -> checkNullPtr (acc ++ checkNullPtr [] notNull [trBr] ++ checkNullPtr [] notNull [elBr] ++ [concatMap (\z -> "dereferencing a pointer that could be null in the " ++ showStmtPos x ++ ": " ++ showExprPos z) (y:ys)]) notNull xs
        IfStmt _ cond trBr Nothing -> case notNull `intersect` extractIdent [cond] [] of
            [] -> checkNullPtr (acc ++ checkNullPtr [] notNull [trBr]) notNull xs
            (y:ys) -> checkNullPtr (acc ++ checkNullPtr [] notNull [trBr] ++ [concatMap (\z -> "dereferencing a pointer that could be null in the " ++ showStmtPos x ++ ": " ++ showExprPos z) (y:ys)]) notNull xs
        WhileStmt _ cond body -> case notNull `intersect` extractIdent [cond] [] of
            [] -> checkNullPtr (acc ++ checkNullPtr [] notNull [body]) notNull xs
            (y:ys) -> checkNullPtr (acc ++ checkNullPtr [] notNull [body] ++ [concatMap (\z -> "dereferencing a pointer that could be null in the " ++ showStmtPos x ++ ": " ++ showExprPos z) (y:ys)]) notNull xs
        DoWhileStmt _ body _ -> checkNullPtr (acc ++ checkNullPtr [] notNull [body]) notNull xs
        ForStmt _ _ _ _ body -> checkNullPtr (acc ++ checkNullPtr [] notNull [body]) notNull xs
        SwitchStmt _ cond body -> case notNull `intersect` extractIdent [cond] [] of
            [] -> checkNullPtr (acc ++ checkNullPtr [] notNull [body]) notNull xs
            (y:ys) -> checkNullPtr (acc ++ checkNullPtr [] notNull [body] ++ [concatMap (\z -> "dereferencing a pointer that could be null in the " ++ showStmtPos x ++ ": " ++ showExprPos z) (y:ys)]) notNull xs
        CaseStmt _ _ body -> checkNullPtr (acc ++ checkNullPtr [] notNull [body]) notNull xs
        DfltStmt _ body -> checkNullPtr (acc ++ checkNullPtr [] notNull [body]) notNull xs
        FnDecStmt _ _ _ body -> checkNullPtr (acc ++ checkNullPtr [] notNull [body]) notNull xs
        LineStmt _ exLst -> checkNullPtr acc (nub (findDeref exLst [] ++ notNull) \\ findAssign exLst []) xs
        BodyStmt stmts -> checkNullPtr acc notNull (stmts ++ xs)
        _ -> checkNullPtr acc notNull xs

checkFor :: [String] -> [Stmt] -> [String]
checkFor acc stmts = case stmts of
    [] -> acc
    (x:xs) -> case x of
        IfStmt _ _ trBr (Just elBr) -> checkFor (acc ++ checkFor [] [trBr] ++ checkFor [] [elBr]) xs
        IfStmt _ _ trBr Nothing -> checkFor (acc ++ checkFor [] [trBr]) xs
        WhileStmt _ end body -> checkFor (acc ++ checkFor [] [body]) xs
        DoWhileStmt _ body end -> checkFor (acc ++ checkFor [] [body]) xs
        ForStmt pos _ end up body -> case end of
            BinOp binSym _ _ -> case binSym of
                Lt -> lessDec up x body xs
                Lte -> lessDec up x body xs
                Gt -> moreInc up x body xs
                Gte -> moreInc up x body xs
                _ -> checkFor acc xs
            _ -> checkFor acc xs
        SwitchStmt _ _ body -> checkFor (acc ++ checkFor [] [body]) xs
        CaseStmt _ _ body -> checkFor (acc ++ checkFor [] [body]) xs
        DfltStmt _ body -> checkFor (acc ++ checkFor [] [body]) xs
        FnDecStmt _ _ _ body -> checkFor (acc ++ checkFor [] [body]) xs
        BodyStmt body -> checkFor acc (body ++ xs)
        _ -> checkFor acc xs
    where lessDec up forStmt body rem = case up of
            UnaOp unaSym _ -> case unaSym of
                    PreDec -> checkFor (acc ++ ["suspicious loop condition: " ++ showStmtPos forStmt] ++ checkFor [] [body]) rem
                    PostDec -> checkFor (acc ++ ["suspicious loop condition: " ++ showStmtPos forStmt] ++ checkFor [] [body]) rem
                    _ -> checkFor (acc ++ checkFor [] [body]) rem
            _ -> checkFor acc rem
          moreInc up forStmt body rem = case up of
                UnaOp unaSym _ -> case unaSym of
                    PreInc -> checkFor (acc ++ ["suspicious loop condition: " ++ showStmtPos forStmt] ++ checkFor [] [body]) rem
                    PostInc -> checkFor (acc ++ ["suspicious loop condition: " ++ showStmtPos forStmt] ++ checkFor [] [body]) rem
                    _ -> checkFor (acc ++ checkFor [] [body]) rem
                _ -> checkFor acc rem

redundBranch :: [String] -> [Stmt] -> [String]
redundBranch acc stmts = case stmts of
    [] -> acc
    (x:xs) -> case x of
        IfStmt {} -> redundBranch (acc ++ [checkRedund [] "branches" (removeElse x []) []]) xs
        WhileStmt _ _ body -> redundBranch (acc ++ redundBranch [] [body]) xs
        DoWhileStmt _ body _ -> redundBranch (acc ++ redundBranch [] [body]) xs
        ForStmt _ _ _ _ body -> redundBranch (acc ++ redundBranch [] [body]) xs
        SwitchStmt _ _ body -> case body of
            BodyStmt body -> redundBranch (acc ++ [checkRedund [] "branches" (removeDflt body []) []]) xs
            _ -> redundBranch acc xs
        FnDecStmt _ _ _ body -> redundBranch (acc ++ redundBranch [] [body]) xs
        BodyStmt body -> redundBranch acc (body ++ xs)
        _ -> redundBranch acc xs
    where removeDflt stmts acc = case stmts of
            [] -> acc
            (x:xs) -> case x of
                CaseStmt _ _ st -> removeDflt xs (acc ++ [st])
                _ -> removeDflt xs acc
          removeElse stmt acc = case stmt of
            IfStmt pos _ st (Just elBr) -> removeElse elBr (acc ++ [st])
            IfStmt pos _ st Nothing -> acc ++ [st]
            _ -> acc

redundCond :: [String] -> [Stmt] -> [String]
redundCond acc stmts = case stmts of
    [] -> acc
    (x:xs) -> case x of
        IfStmt {} -> redundCond (acc ++ [checkRedund [] "conditions" (removeElse x []) []]) xs
        WhileStmt _ _ body -> redundCond (acc ++ redundCond [] [body]) xs
        DoWhileStmt _ body _ ->  redundCond (acc ++ redundCond [] [body]) xs
        ForStmt _ _ _ _ body ->  redundCond (acc ++ redundCond [] [body]) xs
        SwitchStmt _ _ body -> case body of
            BodyStmt body -> redundCond (acc ++ [checkRedund [] "conditions" (removeDflt body []) []]) xs
            _ -> redundCond acc xs
        FnDecStmt _ _ _ body -> redundCond (acc ++ redundCond [] [body]) xs
        BodyStmt body -> redundCond acc (body ++ xs)
        _ -> redundCond acc xs
    where removeDflt stmts acc = case stmts of
            [] -> acc
            (x:xs) -> case x of
                CaseStmt {} -> removeDflt xs (acc ++ [x])
                _ -> removeDflt xs acc
          removeElse stmt acc = case stmt of
              IfStmt pos cond _ (Just elBr) -> removeElse elBr (acc ++ [stmt])
              IfStmt pos cond _ Nothing -> acc ++ [stmt]
              _ -> acc

checkRedund :: String -> String -> [Stmt] -> [Stmt] -> String
checkRedund acc tp body seen = case body of
    [] -> acc
    (x:xs) -> if x `elem` seen then checkRedund (acc ++ "redundant " ++ tp ++ ": " ++ showStmtPos x) tp xs seen
              else checkRedund acc tp xs (x:seen)

showStmtPos :: Stmt -> String
showStmtPos st = case st of
    IfStmt pos _ _ _ -> "if statement at " ++ show pos
    CaseStmt pos _ _ -> "case statement at " ++ show pos
    WhileStmt pos _ _ -> "while statement at" ++ show pos
    DoWhileStmt pos _ _ -> "do-while statement at" ++ show pos
    ForStmt pos _ _ _ _ -> "for statement at " ++ show pos
    SwitchStmt pos _ _ -> "switch statement at " ++ show pos
    DfltStmt pos _ -> "default statement at " ++ show pos
    FnDecStmt pos _ _ _ -> "function declaration at " ++ show pos
    LineStmt pos _ -> "line statement at " ++ show pos
    CntStmt pos -> "continue statement at " ++ show pos
    BrkStmt pos -> "break statement at " ++ show pos
    LabelStmt pos _ -> "label statement at " ++ show pos
    ReturnStmt pos _ -> "return statement at " ++ show pos
    GotoStmt pos _ -> "go-to statement at " ++ show pos
    BodyStmt stmts -> intercalate "\n" $ showStmtPos <$> stmts
    _ -> ""

showExprPos :: Expr -> String
showExprPos ex = case ex of
    PtrMemAcc pos _ _ -> "pointer member access expression at " ++ show pos
    MemAcc pos _ _ -> "member access expression at " ++ show pos
    ArrAcc pos _ _ -> "array element access expression at " ++ show pos
    Ident pos var -> "variable " ++ var ++ " at " ++ show pos
    Call pos name _ -> "function call (function name: " ++ show name ++ " ) at " ++ show pos
    Atom pos val -> "atomic value " ++ show val ++ " at " ++ show pos
    _ -> ""

check :: [[Stmt]] -> String
check stmts = case concat $ unlines . filter (not . null) <$> ([checkFor [], redundBranch [], redundCond [], checkNullPtr [] []] <*> stmts) of 
    "" -> "The checker didn't find any issues!"
    other -> other

checkFile :: IO ()
checkFile = do
    stmts <- getLine >>= parseFile
    writeFile "report.txt" (check [stmts])
    putStrLn "checking completed"
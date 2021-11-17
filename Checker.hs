import CParser
import AST

-- Use (fmap . fmap)

checkFor :: Stmt -> String
checkFor stmt = case stmt of
    BodyStmt stmts -> unwords $ checkFor <$> stmts
    FnDecStmt pos name paraLst body -> checkFor body
    WhileStmt pos ex stmts -> checkFor stmts
    DoWhileStmt pos stmts ex -> checkFor stmts
    SwitchStmt pos ex stmts -> checkFor stmts
    CaseStmt pos ex stmts -> checkFor stmts
    DfltStmt pos stmts -> checkFor stmts
    IfStmt pos cond trBr Nothing -> checkFor trBr
    IfStmt pos cond trBr (Just elBr) -> checkFor trBr ++ "\n" ++ checkFor elBr
    ForStmt pos dec cond up body -> "s"
    _ -> ""

redundBranch :: Stmt -> String 
redundBranch stmt = case stmt of 
    IfStmt pos cond trBr (Just elBr) -> if trBr == elBr then "redundant branches in the \"if\" statement at " ++ show pos else ""
    WhileStmt _ _ body -> redundBranch body 
    DoWhileStmt _ body _ -> redundBranch body 
    ForStmt _ _ _ _ body -> redundBranch body 
    SwitchStmt _ _ body -> redundBranch body 
    CaseStmt _ _ body -> redundBranch body 
    DfltStmt _ body -> redundBranch body 
    FnDecStmt _ _ _ body -> redundBranch body 
    BodyStmt stmts -> unwords $ redundBranch <$> stmts
    _ -> ""


check :: (Stmt -> String) -> String -> IO String
check checker file = fmap (unlines . fmap checker) (parseFile file)

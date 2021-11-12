import Parser
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
    DefaultStmt pos stmts -> checkFor stmts
    IfStmt pos cond trBr Nothing -> checkFor trBr
    IfStmt pos cond trBr (Just elBr) -> checkFor trBr ++ "\n" ++ checkFor elBr
    ForStmt pos dec cond up body -> "s"
    _ -> ""

check :: (Stmt -> String) -> String -> IO String
check checker file = fmap (unlines . fmap checker) (parseFile file)

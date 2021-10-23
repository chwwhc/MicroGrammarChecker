import Parser
import AST

-- Use (fmap . fmap)

checkFor :: Stmt -> String
checkFor stmt = case stmt of
    ForStmt dec cond up body -> "s"
    _ -> ""

import Parser
import AST

-- Use (fmap . fmap)

checkFor :: Stmt -> String
checkFor stmt = case stmt of
    ForStmt pos dec cond up body -> "s"
    _ -> ""

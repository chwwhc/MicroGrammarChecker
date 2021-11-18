import CParser
import AST
import Data.List
import Control.Monad



checkFor :: Stmt -> String
checkFor stmt = ""

redundBranch :: Stmt -> String
redundBranch stmt = case stmt of
    IfStmt {} -> checkRedund "branches" (removeElse stmt []) []
    WhileStmt _ _ body -> redundBranch body
    DoWhileStmt _ body _ -> redundBranch body
    ForStmt _ _ _ _ body -> redundBranch body
    SwitchStmt _ _ body -> case body of
        BodyStmt stmts -> checkRedund "branches" (removeDflt stmts []) []
        _ -> ""
    FnDecStmt _ _ _ body -> redundBranch body
    BodyStmt stmts -> intercalate "" $ redundBranch <$> stmts
    _ -> ""
    where removeDflt stmts acc = case stmts of
            [] -> acc
            (x:xs) -> case x of
                CaseStmt _ _ st -> removeDflt xs (acc ++ [st])
                _ -> removeDflt xs acc
          removeElse stmt acc = case stmt of
              IfStmt pos _ st (Just elBr) -> removeElse elBr (acc ++ [st])
              IfStmt pos _ st Nothing -> acc ++ [st]
              _ -> acc

redundCond :: Stmt -> String
redundCond stmt = case stmt of
    IfStmt {} -> checkRedund "conditions" (removeElse stmt []) []
    WhileStmt _ _ body -> redundCond body
    DoWhileStmt _ body _ -> redundCond body
    ForStmt _ _ _ _ body -> redundCond body
    SwitchStmt _ _ body -> case body of
        BodyStmt stmts -> checkRedund "conditions" (removeDflt stmts []) []
        _ -> ""
    FnDecStmt _ _ _ body -> redundCond body
    BodyStmt stmts -> intercalate "" $ redundCond <$> stmts
    _ -> ""
    where removeDflt stmts acc = case stmts of
            [] -> acc
            (x:xs) -> case x of
                CaseStmt {} -> removeDflt xs (acc ++ [x])
                _ -> removeDflt xs acc
          removeElse stmt acc = case stmt of
              IfStmt pos cond _ (Just elBr) -> removeElse elBr (acc ++ [stmt])
              IfStmt pos cond _ Nothing -> acc ++ [stmt]
              _ -> acc

checkRedund :: String -> [Stmt] -> [Stmt] -> String
checkRedund tp body seen = case body of
    [] -> ""
    (x:xs) -> if x `elem` seen then "redundant " ++ tp ++ ": " ++ showPos x ++ "\n" ++ checkRedund tp xs seen
              else checkRedund tp xs (x:seen)

showPos :: Stmt -> String
showPos st = case st of
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
    BodyStmt stmts -> intercalate "\n" $ showPos <$> stmts
    _ -> ""

check :: String -> IO String
check file = liftM2 (++) (fmap (intercalate "" . removeEmp [] . fmap redundBranch) (parseFile file))
                        (fmap (intercalate "" . removeEmp [] . fmap redundCond) (parseFile file))
    where removeEmp acc lst = case lst of
            [] -> acc
            (x:xs) -> if x == "" then removeEmp acc xs else removeEmp (acc ++ [x]) xs

checkFile :: IO () 
checkFile = getLine >>= check >>= writeFile "report.txt" >> putStrLn "checking completed"
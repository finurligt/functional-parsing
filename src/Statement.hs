module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
import qualified Data.List
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip | Begin [Statement] | While Expr.T Statement
    | Read String | Write Expr.T | Comment
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e
if' = token (accept "if") -# Expr.parse #- token (require "then") # parse #- token(require "else") # parse >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2
skip = token (accept "skip") #- token (require ";") >-> buildSkip
buildSkip x = Skip
begin = token (accept "begin") -# statements #- token (require "end") >-> buildBegin
statements = iter $ token parse
buildBegin s = Begin s
while = token (accept "while") -# Expr.parse #- token (require "do") # parse >-> buildWhile
buildWhile (e, s) = While e s
read' = token (accept "read") -# word #- token (require ";") >-> buildRead
buildRead v = Read v
write = token (accept "write") -# Expr.parse #- token (require ";") >-> buildWrite
buildWrite e = Write e
comment = accept "--" >-> buildComment --TODO lägga till parser för resten av raden
buildComment e = Comment




exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Assignment var expr : stmts) dict input = exec stmts
    (Dictionary.insert (var, Expr.value expr dict) dict) input
exec (Begin beginStmts : stmts) dict input = exec (beginStmts++stmts) dict input
exec (While cond stmt : stmts) dict input =
  if (Expr.value cond dict)>0
    then exec (stmt:(While cond stmt):stmts) dict input
    else exec stmts dict input
exec (Read var : stmts) dict (val:input) = exec stmts (Dictionary.insert (var, val) dict) input
exec (Write expr : stmts) dict input = (Expr.value expr dict) : exec stmts dict input
exec (Skip : stmts) dict input = exec stmts dict input
exec [] _ _ = []

instance Parse Statement where
  parse = assignment ! if' ! skip ! begin ! while ! read' ! write
  toString (Assignment val expr) = val ++ " := " ++ Expr.toString expr ++ ";"
  toString (If cond thenStmt elseStmt) = "if " ++ Expr.toString cond ++ " then\n" ++
    (indent $ toString thenStmt) ++ "\nelse\n" ++ indent (toString elseStmt)
  toString Skip = "skip;"
  toString (Begin stmts) = "begin\n" ++ indent (foldl (\acc stmt -> acc ++ toString stmt ++ "\n") [] stmts) ++ "\nend"
  toString (While expr stmt) = "while " ++ Expr.toString expr ++ " do\n" ++ indent (toString stmt)
  toString (Read val) = "read " ++ val ++ ";"
  toString (Write expr) = "write " ++ Expr.toString expr ++ ";"

indent :: String -> String
indent = Data.List.intercalate "\n" . fmap ("\t" ++) . lines

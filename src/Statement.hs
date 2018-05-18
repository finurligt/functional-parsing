module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip | Begin [Statement] | While Expr.T Statement
    | Read Var | Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e
if' = accept "if" -# Expr.parse #- require "then" # Statement.parse #- require "else" # Statement.parse >-> buildIf
buildIf (e, s1, s2) = If e s1 s2
skip = accept "skip" #- require ";" >-> buildSkip
buildSkip = Skip
begin = accept "begin" -# statements #- require "end" >-> buildBegin
statements = iter Statement.parse
buildBegin s = Begin s
while = accept "while" -# Expr.parse #- require "do" # Statement.parse >-> buildWhile
buildWhile (e, s) = While e s
read' = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v
write =  accept "read" -# Expression.parse #- require ";" >-> buildWrite
buildWrite e = Write e




exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Assignment variable expression) dict input
    dict.insert (variable, Expr.value expression dict)

instance Parse Statement where
  parse = assignment ! if' ! skip ! begin ! while ! read' ! write
  toString = error "Statement.toString not implemented"

module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] -- to be defined
instance Parse T where
  parse = iter Statement.parse #> \stmts -> return (Program stmts)
  toString (Program stmts) = concatMap ((++ "\n") . Statement.toString) stmts

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) input = Statement.exec stmts Dictionary.empty input

{-
exec :: T -> IO ()
exec (Program stmts) = (exec' stmts readInts) >>= printList

readInts :: IO [Integer]
readInts = fmap (parseInts) getContents

parseInts :: String -> [Integer]
parseInts s = map parseInt (words s)

parseInt :: String -> Integer
parseInt s = read s

printList :: [Integer] -> IO ()
printList = putStrLn . unlines . map show

exec' :: [Statement.T] -> IO [Integer] -> IO [Integer]
exec' stmts = fmap (Statement.exec stmts Dictionary.empty)-}

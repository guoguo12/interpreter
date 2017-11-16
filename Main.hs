import Control.Monad
import Control.Monad.Trans.Reader
import System.IO
import Lexer (tokenize)
import Parser (AST (..), parse)

eval :: AST -> String
eval (Number x) = show x
eval (Symbol s) = "'" ++ s
eval (Node _) = "node"

main = forever $ do
    putStr "> "
    hFlush stdout
    input <- getLine
    putStrLn $ eval $ parse $ tokenize (input ++ "\n")

import Control.Monad (forever)
import Control.Monad.Trans.Reader (Reader, runReader, ask, local)
import Data.Map (Map, fromList, insert, lookup)
import System.IO (hFlush, stdout)
import Lexer (tokenize)
import Parser (AST (..), parse)

data Expr = FnExpr (Int -> Int -> Int)
          | NumExpr Int
          | StrExpr String

instance Show Expr where
    show (FnExpr _)  = "<func>"
    show (NumExpr n) = show n
    show (StrExpr s) = "\"" ++ s ++ "\""

type Env = Map String Expr

globalEnv :: Env
globalEnv = fromList
    [ ("+", FnExpr (+))
    , ("-", FnExpr (-))
    , ("*", FnExpr (*))
    , ("/", FnExpr quot)
    , ("min", FnExpr min)
    , ("max", FnExpr max)
    , ("zero", NumExpr 0)
    , ("one", NumExpr 1)
    ]

eval :: AST -> Reader Env Expr
eval (Number n) = return $ NumExpr n
eval (Symbol s) = do
    env <- ask
    case Data.Map.lookup s env of
        Nothing -> error $ "Lookup error: '" ++ s ++ "'"
        Just v  -> return v
eval (Node []) = error "Empty list"
eval (Node (x:xs)) =
    case x of
        (Symbol "let") -> evalLet xs
        _              -> apply x xs

evalLet :: [AST] -> Reader Env Expr
evalLet [Node [], body] = eval body
evalLet [Node ((Node [Symbol name, value]):otherBindings), body] = do
    value' <- eval value
    let localize = insert name value'
    local localize (evalLet (Node otherBindings : [body]))
evalLet _ = error "Invalid let syntax"

apply :: AST -> [AST] -> Reader Env Expr
apply f [arg1, arg2] = do
    f' <- eval f
    x' <- eval arg1
    y' <- eval arg2
    case (f', x', y') of
        (FnExpr fn, NumExpr n1, NumExpr n2) -> return $ NumExpr (fn n1 n2)
        _                                   -> error "Invalid application"
apply _ _ = error "Wrong number of function arguments"

main = forever $ do
    putStr "> "
    hFlush stdout
    input <- getLine
    let ast = parse $ tokenize (input ++ "\n")
    putStrLn $ show $ runReader (eval ast) globalEnv

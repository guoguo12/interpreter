import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import Control.Monad.Trans.Reader (Reader, runReader, ask, local)
import Data.Map (Map, fromList, insert, lookup)
import System.IO (hFlush, stdout)
import Lexer (tokenize)
import Parser (AST (..), parse)

data Expr = NilExpr
          | NumExpr Int
          | StrExpr String
          | PairExpr Expr Expr
          | FnExpr ([Expr] -> Reader Env Expr)

instance Show Expr where
    show NilExpr          = "()"
    show (NumExpr n)      = show n
    show (StrExpr s)      = "\"" ++ s ++ "\""
    show (PairExpr p1 p2) = "(" ++ (show p1) ++ " . " ++ (show p2) ++ ")"
    show (FnExpr _)       = "<function>"

type Env = Map String Expr

globalEnv :: Env
globalEnv = fromList
    [ ("+", makeMathFn2 (+))
    , ("-", makeMathFn2 (-))
    , ("*", makeMathFn2 (*))
    , ("/", makeMathFn2 quot)
    , ("min", makeMathFn2 min)
    , ("max", makeMathFn2 max)
    , ("nil", NilExpr)
    , ("cons", consFn)
    ]

makeMathFn2 :: (Int -> Int -> Int) -> Expr
makeMathFn2 mathFn = FnExpr f
    where f [NumExpr n1, NumExpr n2] = return $ NumExpr $ mathFn n1 n2
          f [_, _]                   = error "Type mismatch (math function)"
          f _                        = error "Arity mismatch (math function)"

consFn :: Expr
consFn = FnExpr f
    where f [p1, p2] = return $ PairExpr p1 p2
          f _        = error "Arity mismatch (cons)"

eval :: AST -> Reader Env Expr
eval (Number n) = return $ NumExpr n
eval (Symbol s) = do
    env <- ask
    case Data.Map.lookup s env of
        Nothing -> error $ "Lookup error: '" ++ s ++ "'"
        Just v  -> return v
eval (Node []) = return NilExpr
eval (Node (x:xs)) =
    case x of
        (Symbol "let")    -> evalLet xs
        (Symbol "lambda") -> evalLambda xs
        _                 -> apply x xs

evalLet :: [AST] -> Reader Env Expr
evalLet [Node [], body] = eval body
evalLet [Node ((Node [Symbol name, value]):otherBindings), body] = do
    value' <- eval value
    let localize = insert name value'
    local localize (evalLet (Node otherBindings : [body]))
evalLet _ = error "Invalid let syntax"

evalLambda :: [AST] -> Reader Env Expr
evalLambda [Node params, body] =
    if not $ checkParams params
    then error "Parameter/identifier was not a symbol"
    else return $ FnExpr (f params)
        where f [] []    = eval body
              f [] (_:_) = error "Arity mismatch (too many arguments)"
              f (_:_) [] = error "Arity mismatch (too few arguments)"
              f (Symbol param:otherParams) (arg:otherArgs) =
                  local (insert param arg) (f otherParams otherArgs)

apply :: AST -> [AST] -> Reader Env Expr
apply f args = do
    f'    <- eval f
    args' <- mapM eval args
    case f' of
        FnExpr fn -> fn args'
        _         -> error "Non-function cannot be applied"

checkParams :: [AST] -> Bool
checkParams = all isSymbol
    where isSymbol (Symbol _) = True
          isSymbol _          = False

main = forever $ do
    putStr "> "
    hFlush stdout
    input <- getLine
    let ast = parse $ tokenize (input ++ "\n")
    catch (putStrLn $ show $ runReader (eval ast) globalEnv)
          (\e -> putStrLn $ show (e :: SomeException))

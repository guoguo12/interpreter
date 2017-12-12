{-# LANGUAGE Rank2Types #-}

import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import Control.Monad.Trans.Reader (Reader, runReader, ask, local)
import Data.Map (Map, fromList, insert, lookup)
import System.Console.Readline (addHistory, readline)
import System.IO (hFlush, stdout)
import Lexer (tokenize)
import Parser (AST (..), parse)

data Expr = NilExpr
          | BoolExpr Bool
          | NumExpr Int
          | StrExpr String
          | PairExpr Expr Expr
          | LambdaExpr ([Expr] -> Expr)
          | MuExpr ([Expr] -> Reader Env Expr)

instance Show Expr where
    show NilExpr          = "()"
    show (BoolExpr b)     = if b then "#t" else "#f"
    show (NumExpr n)      = show n
    show (StrExpr s)      = "\"" ++ s ++ "\""
    show (PairExpr p1 p2) = showPair p1 p2
    show (LambdaExpr _)   = "<#closure>"
    show (MuExpr _)       = "<#dynamic>"

showPair :: Expr -> Expr -> String
showPair p1 p2 = "(" ++ show p1 ++ rest ++ ")"
    where rest = case p2 of
            NilExpr      -> ""
            PairExpr _ _ -> " " ++ (tail $ init $ show p2)
            _            -> " . " ++ show p2

type Env = Map String Expr

globalEnv :: Env
globalEnv = fromList
    [ ("+",    arithmeticPrim (+))
    , ("-",    arithmeticPrim (-))
    , ("*",    arithmeticPrim (*))
    , ("/",    arithmeticPrim quot)
    , ("min",  arithmeticPrim min)
    , ("max",  arithmeticPrim max)
    , ("=",    equalityPrim (==))
    , ("!=",   equalityPrim (/=))
    , ("<",    comparisonPrim (<))
    , ("<=",   comparisonPrim (<=))
    , (">",    comparisonPrim (>))
    , (">=",   comparisonPrim (>=))
    , ("nil",  NilExpr)
    , ("#t",   BoolExpr True)
    , ("#f",   BoolExpr False)
    , ("cons", consFn)
    , ("car",  carFn)
    , ("cdr",  cdrFn)
    , ("list", listFn)
    , ("null?", nullFn)
    ]

arithmeticPrim :: (Int -> Int -> Int) -> Expr
arithmeticPrim fn = LambdaExpr f
    where f [NumExpr n1, NumExpr n2] = NumExpr $ fn n1 n2
          f [_, _]                   = error "Type mismatch (arithmetic fn.)"
          f _                        = error "Arity mismatch (arithmetic fn.)"

equalityPrim :: (forall a. Eq a => a -> a -> Bool) -> Expr
equalityPrim fn = LambdaExpr f
    where f [NumExpr n1, NumExpr n2]   = BoolExpr $ fn n1 n2
          f [BoolExpr b1, BoolExpr b2] = BoolExpr $ fn b1 b2
          f [_, _]                     = error "Type mismatch (equality fn.)"
          f _                          = error "Arity mismatch (equality fn.)"

comparisonPrim :: (Int -> Int -> Bool) -> Expr
comparisonPrim fn = LambdaExpr f
    where f [NumExpr n1, NumExpr n2] = BoolExpr $ fn n1 n2
          f [_, _]                   = error "Type mismatch (comparison fn.)"
          f _                        = error "Arity mismatch (comparison fn.)"

consFn :: Expr
consFn = LambdaExpr f
    where f [p1, p2] = PairExpr p1 p2
          f _        = error "Arity mismatch (cons)"

carFn :: Expr
carFn = LambdaExpr f
    where f [(PairExpr p1 _)] = p1
          f [_]               = error "Type mismatch (car)"
          f _                 = error "Arity mismatch (car)"

cdrFn :: Expr
cdrFn = LambdaExpr f
    where f [(PairExpr _ p2)] = p2
          f [_]               = error "Type mismatch (cdr)"
          f _                 = error "Arity mismatch (cdr)"

listFn :: Expr
listFn = LambdaExpr (foldr PairExpr NilExpr)

nullFn :: Expr
nullFn = LambdaExpr f
    where f [NilExpr] = BoolExpr True
          f _         = BoolExpr False

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
        (Symbol "mu")     -> evalMu xs
        (Symbol "if")     -> evalIf xs
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
    then error "Invalid parameter list for lambda"
    else do
        env <- ask
        return $ LambdaExpr (f env params)
            where f env [] []    = runReader (eval body) env
                  f _   [] (_:_) = error "Arity mismatch (too many arguments)"
                  f _   (_:_) [] = error "Arity mismatch (too few arguments)"
                  f env (Symbol param:otherParams) (arg:otherArgs) =
                      f (insert param arg env) otherParams otherArgs
evalLambda _ = error "Invalid lambda syntax"

evalMu :: [AST] -> Reader Env Expr
evalMu [Node params, body] =
    if not $ checkParams params
    then error "Invalid parameter list for mu"
    else return $ MuExpr (f params)
        where f [] []    = eval body
              f [] (_:_) = error "Arity mismatch (too many arguments)"
              f (_:_) [] = error "Arity mismatch (too few arguments)"
              f (Symbol param:otherParams) (arg:otherArgs) =
                  local (insert param arg) (f otherParams otherArgs)
evalMu _ = error "Invalid mu syntax"

evalIf :: [AST] -> Reader Env Expr
evalIf [condExpr, thenExpr, elseExpr] = do
    condExpr' <- eval condExpr
    case condExpr' of
        BoolExpr False -> eval elseExpr
        _              -> eval thenExpr
evalIf _ = error "Invalid if syntax"

apply :: AST -> [AST] -> Reader Env Expr
apply f args = do
    f'    <- eval f
    args' <- mapM eval args
    case f' of
        LambdaExpr fn -> return $ fn args'
        MuExpr fn     -> fn args'
        _             -> error "Non-function cannot be applied"

checkParams :: [AST] -> Bool
checkParams = all isSymbol
    where isSymbol (Symbol _) = True
          isSymbol _          = False

main :: IO ()
main = do
    input <- readline "> "
    case input of
        Nothing   -> return ()
        Just line -> do
            addHistory line
            let tokens = tokenize (line ++ "\n")
            let ast = parse tokens
            catch (putStrLn $ show $ runReader (eval ast) globalEnv)
                  (\e -> putStrLn $ show (e :: SomeException))
            main

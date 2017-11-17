module Parser (AST (..), parse) where

import Control.Monad.Trans.State (State, evalState, get, put)
import Lexer (tokenize)

numericChars = "0123456789"

isNumeric :: String -> Bool
isNumeric = all (\c -> elem c numericChars)

data AST = Symbol String
         | Number Int
         | Node [AST]
         deriving (Show)

type ParserState = State [String]

parseNext :: ParserState AST
parseNext = do
    tokens <- get
    case tokens of
        []              -> error "Unexpected EOF"
        ("(":remaining) -> do
            put remaining
            tl <- parseTail
            return $ Node tl
        (hd:remaining)  -> do
            put remaining
            return $ if isNumeric hd
                     then Number (read hd :: Int)
                     else Symbol hd

parseTail :: ParserState [AST]
parseTail = do
    tokens <- get
    case tokens of
        []              -> error "Unmatched left paren"
        (")":remaining) -> do
            put remaining
            return []
        _               -> do
            hd <- parseNext
            tl <- parseTail
            return $ hd : tl

parse :: [String] -> AST
parse = evalState parseNext

main = interact $ show . parse . tokenize

module Lexer (tokenize) where

parenChars = "()"
whitespaceChars = " \t\n"
splitChars = parenChars ++ whitespaceChars

tokenize :: String -> [String]
tokenize "" = []
tokenize input =
    let (token, (x:xs)) = span (\x -> notElem x splitChars) input in
    case (token, elem x parenChars) of
        ("", True)  -> [x] : tokenize xs
        ("", False) -> tokenize xs
        (_,  True)  -> token : [x] : tokenize xs
        (_,  False) -> token : tokenize xs

main = interact $ show . tokenize

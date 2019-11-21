module Tokenize
    ( stringToToken
    , cleanString
    )
    where

import Data.Char

data Op = Plus
        | Minus
        | Time
        | Inf
        deriving(Show, Eq)

data Token = Word String
            | Number Int
            | TokenOpen
            | TokenClose
            | TokenOp Op
            deriving(Show, Eq)

cleanString :: String -> String
cleanString [] = []
cleanString s@(';':xs) = cleanString restString
                        where
                            (_, restString) = break counter s
                            counter :: Char -> Bool
                            counter '\n' = True
                            counter _ = False
cleanString (x:xs) = x : cleanString xs

isSpeSpace :: Char -> Bool
isSpeSpace ')' = True
isSpeSpace x = isSpace x 

stringToToken :: String -> [Token]
stringToToken [] = []
stringToToken s@(x:xs)  | x == '(' = TokenOpen : stringToToken xs 
                        | x == ')' = TokenClose : stringToToken xs 
                        | x == '+' = TokenOp Plus : stringToToken xs 
                        | x == '-' = TokenOp Minus : stringToToken xs 
                        | x == '*' = TokenOp Time : stringToToken xs 
                        | x == '<' = TokenOp Inf : stringToToken xs 
                        | isAlpha x = Word word : stringToToken restchar
                        | isSpace x = stringToToken xs
                        | x == '\n' = stringToToken xs
                        | isDigit x = Number (read num :: Int) : stringToToken restnum
                            where
                                (word, restchar) = break isSpeSpace s
                                (num, restnum) = break (not . isDigit) s
stringToToken _ = error "Invalid character"
module Tokenize
    ( stringToToken
    , cleanString
    , reverseList
    , Op(..)
    , Token(..)
    )
    where

import Data.Char

data Op = Plus
        | Minus
        | Time
        | Div
        | Mod
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
isSpeSpace '\'' = True
isSpeSpace x = isSpace x 

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

stringToToken :: String -> [Token]
stringToToken [] = []
stringToToken s@(x:xs)  | x == '(' = TokenOpen : stringToToken xs 
                        | x == ')' = TokenClose : stringToToken xs 
                        | x == '+' = TokenOp Plus : stringToToken xs 
                        | x == '-' = TokenOp Minus : stringToToken xs 
                        | x == '*' = TokenOp Time : stringToToken xs 
                        | x == '<' = TokenOp Inf : stringToToken xs
                        | x == '/' = TokenOp Div : stringToToken xs
                        | x == '\'' = Word "quote" : stringToToken xs 
                        | isAlpha x = isOp word : stringToToken restchar
                        | isSpace x = stringToToken xs
                        | x == '\n' = stringToToken xs
                        | isDigit x = Number (read num :: Int) : stringToToken restnum
                            where
                                (word, restchar) = break isSpeSpace s
                                (num, restnum) = break (not . isDigit) s
                                isOp :: String -> Token
                                isOp "div" = TokenOp Div
                                isOp "mod" = TokenOp Mod
                                isOp w = Word w 
stringToToken _ = error "Invalid character"
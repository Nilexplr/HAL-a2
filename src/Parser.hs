module Parser 
    ( Expr(..)
    , parseExpr
    )
    where

import Tokenize

type Parser a = [Token] -> Maybe(a, [Token])

data Expr = KeyWord String
            | Function Expr
            | Calcul Op [Expr]
            | Val Int
            deriving Show

{-
Parse an expresion value
-}
parseValue :: Parser Expr
parseValue (Number n:xs) = Just (Val n, xs)
-- Launch a parsing instance inside a parenthesis
parseValue (TokenOpen : xs) = case parseValue xs of
    Just (expr, (TokenClose : ys))  -> Just (expr, ys)
    Just _                          -> error "Parse Value never Close"
    Nothing                         -> error "Parse Value return nothing wher token open is detected"
-- Launch a recursive to parse the expressions tab
parseValue (TokenOp op :xs) = Just (Calcul op recursive, rest)
                where
                    (recursive, rest) = parseExprs [] xs
-- Error for parsing the value
parseValue _ = error "Token not recognize"

{-
Parse several expressions
-}
parseExprs :: [Expr] -> [Token] -> ([Expr], [Token])
parseExprs list tokens =
  case parseValue tokens of
    -- All the expressions have been parsed
    Just (expr, []) -> (list ++ [expr], [])
    -- if the next token is a close parenthesis, the recursive is over
    Just (expr, tokens@(TokenClose : xs)) -> (list ++ [expr], tokens)
    -- An expression have been parsed
    Just (expr, x)  -> parseExprs (list ++ [expr]) x
    -- Error during the parsing
    _               -> error "Error during the separation of expretions"

{-
Launch the expression's parsing instance
-}
parseExpr :: [Token] -> [Expr]
parseExpr [] = []
parseExpr tokens = case parseExprs [] tokens of
        (result, []) -> result
        _           -> error "bad parsing"

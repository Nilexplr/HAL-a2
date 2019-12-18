module Parser 
    ( Expr(..)
    , parseExpr
    , parseValue
    )
    where

import Tokenize

type Parser a = [Token] -> Maybe(a, [Token])

data Expr = KeyWord String
            | Function Expr
            | Calcul Op [Expr]
            | Val Int
            deriving Show

-- data Op = ADD
--         | SUB
--         | MUL
--         | DIV
--         | MOD
--     deriving Show

parseValue :: Parser Expr
parseValue (Number n:xs) = Just (Val n, xs)
--
parseValue (TokenOpen : xs) = case parseValue xs of
    Just (expr, (TokenClose : ys))  -> Just (expr, ys)
    Just _                          -> error "Parse Value never Close"
    Nothing                         -> error "Parse Value return nothing wher token open is detected"
--
parseValue (TokenOp op :xs) = Just (Calcul op recursive, rest)
                where
                    (recursive, rest) = parseExprs [] xs
--
parseValue _ = error "Token not recognize"

parseExpr :: [Token] -> [Expr]
parseExpr tokens = case parseExprs [] tokens of
        (result, []) -> result
        _           -> error "bad parsing"

parseExprs :: [Expr] -> [Token] -> ([Expr], [Token])
parseExprs list tokens =
  case parseValue tokens of
    Just (expr, []) -> (list ++ [expr], [])
    Just (expr, tokens@(TokenClose : xs)) -> (list ++ [expr], tokens)
    Just (expr, x)  -> parseExprs (list ++ [expr]) x
    _               -> error "Error during the separation of expretions"
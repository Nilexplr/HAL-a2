module Parser 
    ( Expr

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

parseExpr :: Parser [Expr]
parseExpr s@(x:xs) = case x of
    TokenOpen -> parseExpr xs
    TokenClose -> Just([], xs)
    TokenOp op -> case parseExpr xs of
        Just (a, as) -> case parseOp s of
            Just (b, bs) -> Just ([b], bs)
        _ -> Nothing
    Number i -> case parseExpr xs of
        Just (a, as) -> Just (Val i : a, as)
        _ -> Nothing
    _ -> Nothing
parseExpr _ = Nothing

{- findOp :: [Token] -> [OpExpr] -> Parser OpExpr
 findOp [] _ _ = Nothing
 findOp _ [] _ = Nothing
 findOp _ _ [] = Nothing
 findOp (x:xs) (y:ys) s@(z:zs)   | z == x = Just(y, zs)
                                 | z /= x = findOp xs ys s
 findOp _ _ _ = Nothing -}

parseOp :: Parser Expr
parseOp s@(x:xs) = case x of
    TokenOp op -> case parseExpr xs of
        Just(y, ys) -> Just(Calcul op y, ys)
    _ -> Nothing
parseOp _ = Nothing

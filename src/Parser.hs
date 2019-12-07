module Parser 
    ( Expr

    )
    where

import Tokenize

type Parser a = [Token] -> Maybe(a, [Token])

data Expr = KeyWord String
            | Function Expr

data OpExpr = AddExpr [Expr]
            | SubExpr [Expr]
            | DivExpr [Expr]
            | MulExpr [Expr]
            | ModExpr [Expr]
    deriving Show

-- data Op = ADD
--         | SUB
--         | MUL
--         | DIV
--         | MOD
--     deriving Show

parseExpr :: Parser [Expr]
parseExpr _ = Nothing

findOp :: [TokenOp] -> [OpExpr] -> Parser OpExpr
findOp [] _ _ = Nothing
findOp _ [] _ = Nothing
findOp _ _ [] = Nothing
findOp (x:xs) (y:ys) s@(z:zs)   | z == x = just(y, zs)
                                | z /= x = findOp xs ys s
findOp _ _ _ = Nothing

parseOpExpr :: Parser OpExpr
parseOpExpr [] = Nothing
parseOpExpr s@(x:xs)


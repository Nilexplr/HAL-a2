module Parser 
    ( Expr

    )
    where

import Tokenize

type Parser a = [Token] -> Maybe(a, [Token])

data Expr = KeyWord String
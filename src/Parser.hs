module Parser 
    ( Expr

    )
    where

import Tokenize

type Parser a = [Token] -> Maybe(a, [Token])

data Expr = OperationPrimitive String
          | Keywork String
          | Number Int
          | List [Expr]
          | Function List Expr
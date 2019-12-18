module Eval 
    ( AccessMemory
    , Memory
    , evalFiles
    , evalExpr
    )
    where

import Tokenize
import Parser

-- Create more Memory type can be possible (for exemple instruction)
data Memory = Memory { name :: String, variable :: [Expr], scope :: Expr}

-- Memory that the user can use to save define variable or instructions
data AccessMemory = AccessMemory { access :: [Memory] }


evalExpr :: Expr -> Int
evalExpr (Val nb)           =  nb
evalExpr (Calcul Plus x)    =  sum [evalExpr y | y <- x]
evalExpr (Calcul Plus x)    =  sum [evalExpr y | y <- x]
evalExpr (Calcul Plus x)    =  sum [evalExpr y | y <- x]
evalExpr (Calcul Plus x)    =  sum [evalExpr y | y <- x]

{-
Eval a lisp file to an array of Memory
-}
evalLisp :: String -> [Memory]
evalLisp [] = []
evalLisp (x:xs) = []

{-
Create the AccessMemory
-}
evalFiles :: [String] -> AccessMemory
evalFiles [] = AccessMemory { access = [] }
evalFiles filesList = AccessMemory 
    { 
    access = concat [evalLisp $ filesList !! i | i <- take (length filesList) [0,1..]] 
    }
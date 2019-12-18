module Eval 
    ( AccessMemory
    , Memory
    , evalFiles
    , evalExprString
    , evalExprInt
    )
    where

import Tokenize
import Parser

-- Create more Memory type can be possible (for exemple instruction)
data Memory = Memory { name :: String, variable :: [Expr], scope :: Expr}

-- Memory that the user can use to save define variable or instructions
data AccessMemory = AccessMemory { access :: [Memory] }


{-
TODO:   Create a Type data to merge evalExpr to return differents kind of
        data Types (bool, int, string)        
-}
evalExprInt :: Expr -> Int
evalExprInt (Val nb)            =  nb
evalExprInt (Calcul Plus x)     =  sum       [evalExprInt y | y <- x]
evalExprInt (Calcul Minus x)    =  soustract [evalExprInt y | y <- x]
                                    where
                                        soustract :: [Int] -> Int
                                        soustract (x:[]) = x
                                        soustract (x:(y:rest)) = soustract (x - y:rest)
--
evalExprInt (Calcul Time x)     =  product   [evalExprInt y | y <- x]
evalExprInt (Calcul Div x)      =  divide    [evalExprInt y | y <- x]
                                    where
                                        divide :: [Int] -> Int
                                        divide (x:[]) = x
                                        divide (x:(y:rest)) = divide (quot x y:rest)
--
evalExprInt (Calcul Mod x)      | length x /= 2 = error "Impossible to Modulo more than 2 numbers"
                                | otherwise     = (evalExprInt $ x !! 0) `mod` (evalExprInt $ x !! 1)
                            

{-
Eval an Expression
-}
evalExprString :: Expr -> String
evalExprString (Val nb)             =  show nb
--
evalExprString (Calcul Inf x)       | length x /= 2 = error "Impossible to compare more than 2 numbers"
                                    | (evalExprInt $ x !! 0) < (evalExprInt $ x !! 1)   = "#t"
                                    | otherwise                                         = "#f"
--
evalExprString expr@(Calcul _ _)    =  show $ evalExprInt expr


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
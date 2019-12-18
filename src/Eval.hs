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
TODO:   Implement the AccesMemory to the evalExpr to look inside when a word is detect
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
evalExprString (Calcul Inf x)       | length x /= 2     = error "Impossible to compare more than 2 numbers"
                                    | (evalExprInt $ x !! 0) < (evalExprInt $ x !! 1)   = "#t"
                                    | otherwise                                         = "#f"
--
evalExprString expr@(Calcul _ _)    =  show $ evalExprInt expr
--
evalExprString (Symbol "quote" x)   | length x /= 1     = error "Invalid agrument for quote"
                                    | otherwise         = displayExpr $ x !! 0
                                        where
                                            displayExpr :: Expr -> String
                                            displayExpr (Val nb)            = show nb
                                            displayExpr (List exprs)        = "(" ++ displayExpr (head exprs) ++ concat [" " ++ displayExpr x | x <- (tail exprs)] ++ ")"
                                            displayExpr (Calcul op exprs)   = "(" ++ ")"
                                            displayExpr (Symbol name exprs)   = "(" ++ name ++ concat [" " ++ displayExpr x | x <- exprs] ++ ")"
--

--
evalExprString _                    = error "Impossible to evaluate expression"


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
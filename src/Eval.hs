module Eval 
    ( AccessMemory
    , Memory
    , evalFiles
    , evalExpr
    , evalArithmetic
    )
    where

import Tokenize
import Parser

-- Create more Memory type can be possible (for exemple instruction)
data Memory = Memory { name :: String, variable :: [Expr], scope :: Expr}

-- Memory that the user can use to save define variable or instructions
data AccessMemory = AccessMemory { access :: [Memory] }



displayExpr :: Expr -> String
displayExpr (Val nb)            = show nb
displayExpr (List exprs)        = "(" ++ displayExpr (head exprs) ++ concat [" " ++ displayExpr x | x <- (tail exprs)] ++ ")"
displayExpr (Calcul op exprs)   = "(" ++ show op ++ concat [" " ++ displayExpr x | x <- exprs] ++  ")"
displayExpr (Symbol "'" exprs)  = "(" ++ "'" ++ (displayExpr $ head exprs) ++concat [" " ++ displayExpr x | x <- tail exprs] ++ ")"
displayExpr (Symbol name exprs) = "(" ++ name ++ concat [" " ++ displayExpr x | x <- exprs] ++ ")"

{-
TODO:   Create a Type data to merge evalExpr to return differents kind of
        data Types (bool, int, string)
TODO:   Implement the AccesMemory to the evalExpr to look inside when a word is detect
-}
evalArithmetic :: Expr -> Int
evalArithmetic (Val nb)            =  nb
evalArithmetic (Calcul Plus x)     =  sum       [evalArithmetic y | y <- x]
evalArithmetic (Calcul Minus x)    =  soustract [evalArithmetic y | y <- x]
                                    where
                                        soustract :: [Int] -> Int
                                        soustract (x:[]) = x
                                        soustract (x:(y:rest)) = soustract (x - y:rest)
--
evalArithmetic (Calcul Time x)     =  product   [evalArithmetic y | y <- x]
evalArithmetic (Calcul Div x)      =  divide    [evalArithmetic y | y <- x]
                                    where
                                        divide :: [Int] -> Int
                                        divide (x:[]) = x
                                        divide (x:(y:rest)) = divide (quot x y:rest)
--
evalArithmetic (Calcul Mod x)      | length x /= 2 = error "Impossible to Modulo more than 2 numbers"
                                   | otherwise     = (evalArithmetic $ x !! 0) `mod` (evalArithmetic $ x !! 1)
                            

{-
Eval an Expression
-}
evalExpr :: Expr -> String
evalExpr (Val nb)             =  show nb
--
evalExpr (Calcul Inf x)         | length x /= 2     = error "Impossible to compare more than 2 numbers"
                                | (evalArithmetic $ x !! 0) < (evalArithmetic $ x !! 1)   = "#t"
                                | otherwise                                               = "#f"
--
evalExpr expr@(Calcul _ _)    =  show $ evalArithmetic expr
--
evalExpr (Symbol "quote" x)         | length x /= 1     = error "Invalid argument for quote"
                                    | otherwise         = displayExpr $ x !! 0
--
evalExpr (Symbol "'" x)             | length x /= 1     = error "Invalid argument for quote"
                                    | otherwise         = displayExpr $ x !! 0
--

--
evalExpr _                    = error "Impossible to evaluate expression"


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
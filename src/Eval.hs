module Eval 
    ( AccessMemory(..)
    , Memory(..)
    , evalFiles
    , evalExpr
    , evalArithmetic
    , displayExpr
    , giveExpr
    )
    where

import Tokenize
import Parser

-- Create more Memory type can be possible (for exemple instruction)
data Memory = Memory { name :: String, scope :: Expr}

-- Memory that the user can use to save define variable or instructions
type AccessMemory = [Memory]

displayExpr :: Expr -> String
displayExpr (Val nb)                    = show nb
displayExpr (KeyWord x)                 = x
displayExpr (Procedure _)               = "#<procedure>"
displayExpr (List [])                   = "()"
displayExpr (CellList [])               = "()"
displayExpr (List exprs@(x:xs))         = "(" ++ displayExpr (head exprs) ++ concat [" " ++ displayExpr x | x <- (tail exprs)] ++ ")"
--
displayExpr (CellList exprs@(x:xs))     = case exprs !! 0 of
        Val y       -> case (exprs !! 1) of
            List ys     ->  displayExpr $ List (x:ys)
            CellList ys ->  displayExpr $ List (x:ys)
            Val  ys     ->  "(" ++ displayExpr x ++ " . " ++ displayExpr (exprs !! 1) ++ ")"
            KeyWord ys  ->  "(" ++ displayExpr x ++ " . " ++ displayExpr (exprs !! 1) ++ ")"
        List y      -> case (exprs !! 1) of
            List ys     ->  displayExpr $ List ([List y] ++ ys)
            CellList ys ->  displayExpr $ List ([List y] ++ ys)
            Val  ys     ->  "(" ++ displayExpr x ++ " . " ++ displayExpr (exprs !! 1) ++ ")"
            KeyWord ys  ->  "(" ++ displayExpr x ++ " . " ++ displayExpr (exprs !! 1) ++ ")"
        CellList y  -> case (exprs !! 1) of    
            List ys     ->  displayExpr $ List ([List y] ++ ys)
            CellList ys ->  displayExpr $ List ([List y] ++ ys)
            Val  ys     ->  "(" ++ displayExpr x ++ " . " ++ displayExpr (exprs !! 1) ++ ")"
            KeyWord ys  ->  "(" ++ displayExpr x ++ " . " ++ displayExpr (exprs !! 1) ++ ")"
        KeyWord y   -> case (exprs !! 1) of
            List ys     ->  displayExpr $ List ([x] ++ ys)
            CellList ys ->  displayExpr $ List ([x] ++ ys)
            Val  ys     ->  "(" ++ displayExpr x ++ " . " ++ displayExpr (exprs !! 1) ++ ")"
            KeyWord ys  ->  "(" ++ displayExpr x ++ " . " ++ displayExpr (exprs !! 1) ++ ")"
            
--
displayExpr (Calcul op exprs)           = "(" ++ drawOp op ++ concat [" " ++ displayExpr x | x <- exprs] ++  ")"
displayExpr (Symbol "'" exprs)          = "(" ++ "'" ++ (displayExpr $ head exprs) ++concat [" " ++ displayExpr x | x <- tail exprs] ++ ")"
displayExpr (Symbol name exprs)         = "(" ++ name ++ concat [" " ++ displayExpr x | x <- exprs] ++ ")"

{-
Eval an arithmetic expression
-}
evalArithmetic :: AccessMemory -> Expr -> Int
evalArithmetic ram (Val nb)            =  nb
evalArithmetic ram (Calcul Plus x)     =  sum       [evalArithmetic ram y | y <- x]
evalArithmetic ram (Calcul Minus x)    |  length x == 1 = - (evalArithmetic ram (x !! 0))
                                   | otherwise = soustract [evalArithmetic ram y | y <- x]
                                    where
                                        soustract :: [Int] -> Int
                                        soustract (x:[]) = x
                                        soustract (x:(y:rest)) = soustract (x - y:rest)
--
evalArithmetic ram (Calcul Time x)     =  product   [evalArithmetic ram y | y <- x]
evalArithmetic ram (Calcul Div x)      =  divide    [evalArithmetic ram y | y <- x]
                                    where
                                        divide :: [Int] -> Int
                                        divide (x:[]) = x
                                        divide (x:(y:rest)) = divide (quot x y:rest)
--
evalArithmetic ram (Calcul Mod x)      | length x /= 2 = error "Impossible to Modulo more than 2 numbers"
                                   | otherwise     = (evalArithmetic ram (x !! 0)) `mod` (evalArithmetic ram (x !! 1))
                            

giveExpr :: (AccessMemory, Expr) -> Expr
giveExpr (ram, expr) = expr
{-
Eval an Expression

TODO:   Implement the AccesMemory to the evalExpr to look inside when a word is detect
        And to stock define data
-}
evalExpr :: AccessMemory -> Expr -> (AccessMemory, Expr)
evalExpr ram (Val nb)               = (ram, Val nb)
evalExpr ram (KeyWord x)            = (ram, KeyWord x)
evalExpr ram (List x)               = (ram, List x)
--
evalExpr ram (Calcul Inf x)         | length x /= 2     = error "Impossible to compare more than 2 numbers"
                                    | (evalArithmetic ram (x !! 0)) < (evalArithmetic ram (x !! 1))     = (ram, KeyWord "#t")
                                    | otherwise                                                         = (ram, KeyWord "#f")
--
evalExpr ram expr@(Calcul _ _)      =  (ram, Val (evalArithmetic ram expr))
--
evalExpr ram (Symbol "quote" x)     | length x /= 1     = error "Invalid argument for quote"
                                    | otherwise         = (ram, x !! 0)
--
evalExpr ram (Symbol "'" x)         | length x /= 1     = error "Invalid argument for quote"
                                    | otherwise         = (ram, x !! 0)
--
evalExpr ram (Symbol "cons" x)      | length x /= 2     = error "Invalid argument for cons"
                                    | otherwise         = evalExpr ram (CellList (expr0: [expr1]))
                                        where
                                            (_, expr0) = (evalExpr ram (x !! 0))
                                            (_, expr1) = (evalExpr ram (x !! 1))
--
evalExpr ram (Symbol "car" x)       | length x /= 1     = error "Invalid argument for car"
                                    | otherwise         = case evalExpr ram (head $ x) of
                                        (ram, CellList (y:_)) ->  (ram, y)
                                        (ram, List (y:_))     ->  (ram, y)

--
evalExpr ram (Symbol "cdr" x)       | length x /= 1     = error "Invalid argument for cdr"
                                    | otherwise         = case evalExpr ram (head $ x) of
                                        (ram, CellList y)      -> (ram ,y !! 1)
                                        (ram, List y)          -> (ram ,List $ tail $ y)
--
evalExpr ram (Symbol "list" x)      | length x == 0     = error "Invalid argument for list"
                                    | otherwise         = (ram, List [giveExpr (evalExpr ram expr) | expr <- x])
                                            
--
evalExpr ram (Symbol "eq?" x)       | length x /= 2     = error "Invalid argument for eq?"
                                    | otherwise         = case evalExpr ram (head $ x) of
                                        (ram, List [])     -> case evalExpr ram (x !! 1) of
                                            (ram, List [])     -> (ram, KeyWord "#t")
                                            _           -> (ram, KeyWord "#f")
                                        (ram, Val a)       -> case evalExpr ram (x !! 1) of
                                            (ram, Val b)       -> (ram, if a == b then KeyWord "#t" else KeyWord "#f")
                                            _           -> (ram, KeyWord "#f")
                                        (ram, KeyWord a)   -> case evalExpr ram (x !! 1) of
                                            (ram, KeyWord b)   -> (ram, if a == b then KeyWord "#t" else KeyWord "#f")
                                            _           -> (ram, KeyWord "#f")
                                        _           -> (ram, KeyWord "#f")
--
evalExpr ram (Symbol "atom?" x)     | length x /= 1     = error "Invalid argument for atom?"
                                    | otherwise         = case evalExpr ram (head $ x) of
                                        (ram, Val _)           -> (ram, KeyWord "#t")
                                        (ram, KeyWord _)       -> (ram, KeyWord "#t")
                                        (ram, List [])         -> (ram, KeyWord "#t")
                                        _               -> (ram, KeyWord "#f")
--
evalExpr ram (Symbol "cond" x)      | length x == 0     = error "Invalid argument for cond"
                                    | otherwise         = case evalExpr ram (head $ x) of
                                        (ram, List (y:ys))     -> case evalExpr ram y of
                                            (ram, KeyWord "#t")    -> (evalExpr ram (ys !! 0))
                                            _               -> (evalExpr ram $ (Symbol "cond" (tail x)))
                                        (ram, CellList (y:ys)) -> case evalExpr ram y of
                                            (ram, KeyWord "#t")    -> (evalExpr ram (ys !! 0))
                                            _               -> (evalExpr ram $ (Symbol "cond" (tail x)))
                                        otherwise       -> error "Impossible to evaluate cond"
--
evalExpr ram (Symbol "lambda" x)    | length x /= 2     = error "Invalid argument for lambda"
                                    | otherwise         = (ram, Procedure (x !! 0, x !! 1))  
--
evalExpr ram (Symbol "define" x)    | length x /= 2     = error "Invalid argument for lambda"
                                    | otherwise         = (ram, Procedure (x !! 0, x !! 1)) 
--
evalExpr ram (Symbol "let" x)       | length x /= 2     = error "Invalid argument for lambda"
                                    | otherwise         = (ram, Procedure (x !! 0, x !! 1)) 
--
-- evalExpr ram (List x)               | x == 
--
evalExpr ram expr@(CellList x)      = case x of
    (Val n:(List y:[]))             -> (ram, List (Val n:y))
    (Val n:(Val y:[]))              -> (ram, expr)
    (Val n:(CellList y:[]))         -> (ram, expr)
    (KeyWord n:(KeyWord y:[]))      -> (ram, expr)
    (KeyWord n:(CellList y:[]))     -> (ram, expr)
    (KeyWord n:(List y:[]))         -> (ram, List (KeyWord n:y))
    (List n:(List y:[]))            -> (ram, List (List n:y))
    (List n:(KeyWord y:[]))         -> (ram, expr)
    (List n:(Val y:[]))             -> (ram, expr)

--
evalExpr ram _                      = error "Impossible to evaluate expression"


{-
Eval a lisp file to an array of Memory
-}
evalLisp :: String -> [Memory]
evalLisp [] = []
evalLisp filename = []

{-
Create the AccessMemory
-}
evalFiles :: [String] -> AccessMemory
evalFiles [] = []
evalFiles filesList = concat [evalLisp $ filesList !! i | i <- take (length filesList) [0,1..]] 
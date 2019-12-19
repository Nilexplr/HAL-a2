module Eval 
    ( AccessMemory(..)
    , Memory(..)
    , evalFiles
    , evalExpr
    , evalArithmetic
    , displayExpr
    )
    where

import Tokenize
import Parser

-- Create more Memory type can be possible (for exemple instruction)
data Memory = Memory { name :: String, variable :: [Expr], scope :: Expr}

-- Memory that the user can use to save define variable or instructions
data AccessMemory = AccessMemory { access :: [Memory] }

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
                            

{-
Eval an Expression

TODO:   Implement the AccesMemory to the evalExpr to look inside when a word is detect
        And to stock define data
-}
evalExpr :: AccessMemory -> Expr -> Expr
evalExpr ram (Val nb)               = Val nb
evalExpr ram (KeyWord x)            = KeyWord x
evalExpr ram (List x)               = List x
--
evalExpr ram (Calcul Inf x)         | length x /= 2     = error "Impossible to compare more than 2 numbers"
                                    | (evalArithmetic ram (x !! 0)) < (evalArithmetic ram (x !! 1))     = KeyWord "#t"
                                    | otherwise                                                         = KeyWord "#f"
--
evalExpr ram expr@(Calcul _ _)      =  Val (evalArithmetic ram expr)
--
evalExpr ram (Symbol "quote" x)     | length x /= 1     = error "Invalid argument for quote"
                                    | otherwise         = x !! 0
--
evalExpr ram (Symbol "'" x)         | length x /= 1     = error "Invalid argument for quote"
                                    | otherwise         = x !! 0
--
evalExpr ram (Symbol "cons" x)      | length x /= 2     = error "Invalid argument for cons"
                                    | otherwise         = evalExpr ram (CellList ((evalExpr ram (x !! 0)): [evalExpr ram (x !! 1)]))
--
evalExpr ram (Symbol "car" x)       | length x /= 1     = error "Invalid argument for car"
                                    | otherwise         = case evalExpr ram (head $ x) of
                                        CellList (y:_) ->  y
                                        List (y:_)     ->  y

--
evalExpr ram (Symbol "cdr" x)       | length x /= 1     = error "Invalid argument for cdr"
                                    | otherwise         = case evalExpr ram (head $ x) of
                                        CellList y      -> y !! 1
                                        List y          -> List $ tail $ y 
--
evalExpr ram (Symbol "list" x)      | length x == 0     = error "Invalid argument for list"
                                    | otherwise         = List [evalExpr ram expr | expr <- x]
--
evalExpr ram (Symbol "eq?" x)       | length x /= 2     = error "Invalid argument for eq?"
                                    | otherwise         = case evalExpr ram (head $ x) of
                                        List []     -> case evalExpr ram (x !! 1) of
                                            List []     -> KeyWord "#t"
                                            _           -> KeyWord "#f"
                                        Val a       -> case evalExpr ram (x !! 1) of
                                            Val b       -> if a == b then KeyWord "#t" else KeyWord "#f"
                                            _           -> KeyWord "#f"
                                        KeyWord a   -> case evalExpr ram (x !! 1) of
                                            KeyWord b   -> if a == b then KeyWord "#t" else KeyWord "#f"
                                            _           -> KeyWord "#f"
                                        _           -> KeyWord "#f"
--
evalExpr ram (Symbol "atom?" x)     | length x /= 1     = error "Invalid argument for atom?"
                                    | otherwise         = case evalExpr ram (head $ x) of
                                        Val _           -> KeyWord "#t"
                                        KeyWord _       -> KeyWord "#t"
                                        List []         -> KeyWord "#t"
                                        _               -> KeyWord "#f"
--
evalExpr ram (Symbol "cond" x)      | length x == 0     = error "Invalid argument for cond"
                                    | otherwise         = case evalExpr ram (head $ x) of
                                        List (y:ys)     -> case evalExpr ram y of
                                            KeyWord "#t"    -> evalExpr ram (ys !! 0)
                                            _               -> evalExpr ram $ (Symbol "cond" (tail x))
                                        CellList (y:ys) -> case evalExpr ram y of
                                            KeyWord "#t"    -> evalExpr ram (ys !! 0)
                                            _               -> evalExpr ram $ (Symbol "cond" (tail x))
                                        otherwise       -> error "Impossible to evaluate cond"
--
evalExpr ram (Symbol "lambda" x)    | length x /= 2     = error "Invalid argument for lambda"
                                    | otherwise         = Procedure (x !! 0, x !! 1)  
--
evalExpr ram (Symbol "define" x)    | length x /= 2     = error "Invalid argument for lambda"
                                    | otherwise         = Procedure (x !! 0, x !! 1) 
--
evalExpr ram (Symbol "let" x)       | length x /= 2     = error "Invalid argument for lambda"
                                    | otherwise         = Procedure (x !! 0, x !! 1) 
--
-- evalExpr ram (List x)               | x == 
--
evalExpr ram expr@(CellList x)      = case x of
    (Val n:(List y:[]))         -> List (Val n:y)
    (Val n:(Val y:[]))          -> expr
    (Val n:(CellList y:[]))     -> expr
    (KeyWord n:(KeyWord y:[]))  -> expr
    (KeyWord n:(CellList y:[])) -> expr
    (KeyWord n:(List y:[]))     -> List (KeyWord n:y)
    (List n:(List y:[]))        -> List (List n:y)
    (List n:(KeyWord y:[]))     -> expr
    (List n:(Val y:[]))         -> expr

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
evalFiles [] = AccessMemory { access = [] }
evalFiles filesList = AccessMemory 
    { 
    access = concat [evalLisp $ filesList !! i | i <- take (length filesList) [0,1..]] 
    }
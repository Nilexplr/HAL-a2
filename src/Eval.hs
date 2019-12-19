module Eval 
    ( AccessMemory
    , Memory
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
displayExpr (List [])                   = "()"
displayExpr (CellList [])               = "()"
displayExpr (List exprs@(x:xs))         = "(" ++ displayExpr (head exprs) ++ concat [" " ++ displayExpr x | x <- (tail exprs)] ++ ")"
--
displayExpr (CellList exprs@(x:xs))     = case exprs !! 0 of
        Val y       -> case (exprs !! 1) of
            List ys     ->  displayExpr $ List ([x] ++ ys)
            CellList ys ->  displayExpr $ List ([x] ++ ys)
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
displayExpr (Calcul op exprs)           = "(" ++ show op ++ concat [" " ++ displayExpr x | x <- exprs] ++  ")"
displayExpr (Symbol "'" exprs)          = "(" ++ "'" ++ (displayExpr $ head exprs) ++concat [" " ++ displayExpr x | x <- tail exprs] ++ ")"
displayExpr (Symbol name exprs)         = "(" ++ name ++ concat [" " ++ displayExpr x | x <- exprs] ++ ")"

{-
Eval an arithmetic expression
-}
evalArithmetic :: Expr -> Int
evalArithmetic (Val nb)            =  nb
evalArithmetic (Calcul Plus x)     =  sum       [evalArithmetic y | y <- x]
evalArithmetic (Calcul Minus x)    |  length x == 1 = - (evalArithmetic $ x !! 0)
evalArithmetic (Calcul Minus x)    | otherwise = soustract [evalArithmetic y | y <- x]
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

TODO:   Implement the AccesMemory to the evalExpr to look inside when a word is detect
        And to stock define data
-}
evalExpr :: Expr -> Expr
evalExpr (Val nb)               = Val nb
evalExpr (KeyWord x)            = KeyWord x
evalExpr (List x)               = List x
--
evalExpr (Calcul Inf x)         | length x /= 2     = error "Impossible to compare more than 2 numbers"
                                | (evalArithmetic $ x !! 0) < (evalArithmetic $ x !! 1)   = KeyWord "#t"
                                | otherwise                                               = KeyWord "#f"
--
evalExpr expr@(Calcul _ _)      =  Val (evalArithmetic $ expr)
--
evalExpr (Symbol "quote" x)     | length x /= 1     = error "Invalid argument for quote"
                                | otherwise         = x !! 0
--
evalExpr (Symbol "'" x)         | length x /= 1     = error "Invalid argument for quote"
                                | otherwise         = x !! 0
--
evalExpr (Symbol "cons" x)      | length x /= 2     = error "Invalid argument for cons"
                                | otherwise         = CellList ((evalExpr $ x !! 0 ): [evalExpr $ x !! 1])
--
evalExpr (Symbol "car" x)       | length x /= 1     = error "Invalid argument for car"
                                | otherwise         = case evalExpr $ head $ x of
                                    CellList (y:_) ->  y
                                    List (y:_)     ->  y

--
evalExpr (Symbol "cdr" x)       | length x /= 1     = error "Invalid argument for cdr"
                                | otherwise         = case evalExpr $ head $ x of
                                    CellList y      -> y !! 1
                                    List y          -> List $ tail $ y 
--
evalExpr (Symbol "list" x)      | length x == 0     = error "Invalid argument for cdr"
                                | otherwise         = List [evalExpr expr | expr <- x]
--
evalExpr (Symbol "eq?" x)       | length x /= 2     = error "Invalid argument for eq?"
                                | otherwise         = case evalExpr $ head $ x of
                                    List [] -> case evalExpr $ x !! 1 of
                                        List [] -> KeyWord "#t"
                                        _       -> KeyWord "#f"
                                    Val a -> case evalExpr $ x !! 1 of
                                        Val b   -> if a == b then KeyWord "#t" else KeyWord "#f"
                                        _       -> KeyWord "#f"
                                    KeyWord a -> case evalExpr $ x !! 1 of
                                        KeyWord b   -> if a == b then KeyWord "#t" else KeyWord "#f"
                                        _           -> KeyWord "#f"
                                    _         -> KeyWord "#f"
--
evalExpr (Symbol "atom?" x)     | length x /= 1     = error "Invalid argument for atom?"
                                | otherwise         = case evalExpr $ head $ x of
                                    Val _           -> KeyWord "#t"
                                    KeyWord _       -> KeyWord "#t"
                                    List []         -> KeyWord "#t"
                                    _               -> KeyWord "#f"
--
evalExpr (Symbol "cond" x)      | length x == 0     = error "Invalid argument for cond"
                                | otherwise         = case evalExpr $ head $ x of
                                    List (y:ys)     -> case evalExpr $ y of
                                        KeyWord "#t"    -> evalExpr $ ys !! 0
                                        _               -> evalExpr $ (Symbol "cond" (tail x))
                                    CellList (y:ys) -> case evalExpr $ y of
                                        KeyWord "#t"    -> evalExpr $ ys !! 0
                                        _               -> evalExpr $ (Symbol "cond" (tail x))
                                    otherwise       -> error "Impossible to evaluate cond"
--
evalExpr _                      = error "Impossible to evaluate expression"


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
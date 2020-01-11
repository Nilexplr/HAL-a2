module Eval 
    ( AccessMemory(..)
    , Memory(..)
    , evalLisp
    , evalExpr
    , evalArithmetic
    , displayExpr
    , giveExpr
    , giveAccessMemory
    )
    where

import Tokenize
import Parser

-- Create more Memory type can be possible (for exemple instruction)
data Memory = Memory { name :: String, scope :: Expr} deriving (Show, Eq)

-- Memory that the user can use to save define variable or instructions
type AccessMemory = [Memory]

{-

-}
addToAccess :: AccessMemory -> Memory -> AccessMemory
addToAccess [] new  = [new]
addToAccess list new    | length [x | x <- list, name x == name new] /= 0 = [x | x <- list, name x /= name new] ++ [new]
                        | length [x | x <- list, name x == name new] == 0 = list ++ [new]
addToAccess _ new = error ("Can not add memory to access memory " ++ (show $ new))

{-

-}
getInAccess :: AccessMemory -> String -> Expr
getInAccess [] cible = error ("Variable " ++ cible ++ " not in bound")
getInAccess list cible | length [x | x <- list, name x == cible] >= 1 = scope (last [x | x <- list, name x == cible])
getInAccess _ cible = error ("Variable " ++ cible ++ " not in bound")

{-

-}
transformExprToMemory :: Expr -> Memory
transformExprToMemory (List (KeyWord symbol: (y:[])))   = Memory { name = symbol, scope = y}
transformExprToMemory (List (List args: (y:[])))        = case args of
    (KeyWord symbol: rest)      ->  Memory { name = symbol, scope = Procedure (List rest, y)}

--
transformExprToMemory _ = error "Can not transform expression to memory"
{-

-}
giveExpr :: (AccessMemory, Expr) -> Expr
giveExpr (ram, expr) = expr

{-

-}
giveAccessMemory :: (AccessMemory, Expr) -> AccessMemory
giveAccessMemory (ram, expr) = ram

{-

-}
displayExpr :: Expr -> String
displayExpr (Val nb)                    = show nb
displayExpr (KeyWord x)                 = x
displayExpr (Procedure x)               = "#<procedure>"
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
evalArithmetic ram (KeyWord x)         =  evalArithmetic ram (getInAccess ram x)
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
--
evalArithmetic ram x                   = evalArithmetic ram $ giveExpr $ evalExpr ram $ x

{-
Eval an Expression
-}
evalExpr :: AccessMemory -> Expr -> (AccessMemory, Expr)
evalExpr ram (Val nb)               = (ram, Val nb)
evalExpr ram (Procedure x)          = (ram, Procedure x)
evalExpr ram expr@(KeyWord ('#':y)) = (ram, expr)
evalExpr ram (KeyWord x)            = (ram, (getInAccess ram x))
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
                                        (new, CellList (y:_)) ->  (new, y)
                                        (new, List (y:_))     ->  (new, y)
                                        (new, x)              -> error ("Error in car command because of " ++ show x)

--
evalExpr ram (Symbol "cdr" x)       | length x /= 1     = error "Invalid argument for cdr"
                                    | otherwise         = case evalExpr ram (head $ x) of
                                        (new, CellList y)      -> (new ,y !! 1)
                                        (new, List (y:[]))     -> (new ,List [])
                                        (new, List y)          -> (new ,List $ tail $ y)
                                        (new, x)               -> error ("Error in cdr command because of " ++ show x)
--
evalExpr ram (Symbol "list" x)      | length x == 0     = error "Invalid argument for list"
                                    | otherwise         = (ram, List [giveExpr (evalExpr ram expr) | expr <- x])
                                            
--
evalExpr ram (Symbol "eq?" x)       | length x /= 2     = error "Invalid argument for eq?"
                                    | otherwise         = case evalExpr ram (head $ x) of
                                        (new, List [])     -> case evalExpr new (x !! 1) of
                                            (new, List [])      -> (new, KeyWord "#t")
                                            (new, _)            -> (new, KeyWord "#f")
                                        (new, Val a)       -> case evalExpr new (x !! 1) of
                                            (new, Val b)        -> (new, if a == b then KeyWord "#t" else KeyWord "#f")
                                            (new, _)            -> (new, KeyWord "#f")
                                        (new, KeyWord a)   -> case evalExpr new (x !! 1) of
                                            (new, KeyWord b)    -> (new, if a == b then KeyWord "#t" else KeyWord "#f")
                                            (new, _)            -> (new, KeyWord "#f")
                                        (new, _)           -> (new, KeyWord "#f")
--
evalExpr ram (Symbol "atom?" x)     | length x /= 1     = error "Invalid argument for atom?"
                                    | otherwise         = case evalExpr ram (head $ x) of
                                        (new, Val _)            -> (new, KeyWord "#t")
                                        (new, KeyWord _)        -> (new, KeyWord "#t")
                                        (new, List [])          -> (new, KeyWord "#t")
                                        (new, _)                -> (new, KeyWord "#f")
--
evalExpr ram (Symbol "cond" x)      | length x == 0     = error "Invalid argument for cond"
                                    | otherwise         = case evalExpr ram (head $ x) of
                                        (_, List (y:ys))      -> case evalExpr ram y of
                                            (_, KeyWord "#t")     -> (evalExpr ram (ys !! 0))
                                            _                       -> (evalExpr ram $ (Symbol "cond" (tail x)))
                                        (_, CellList (y:ys))  -> case evalExpr ram y of
                                            (_, KeyWord "#t")     -> (evalExpr ram (ys !! 0))
                                            _                       -> (evalExpr ram $ (Symbol "cond" (tail x)))
                                        otherwise       -> error "Impossible to evaluate cond"
--
evalExpr ram (Symbol "lambda" x)    | length x /= 2     = error "Invalid argument for lambda"
                                    | otherwise         = (ram, Procedure (x !! 0, x !! 1)) 
--
evalExpr ram (Symbol "define" x)    | length x /= 2     = error "Invalid argument for define"
                                    | otherwise         = (addToAccess ram mem, KeyWord $ name $ mem)
                                            where
                                                mem = transformExprToMemory (List x)
--
evalExpr ram (Symbol "let" x)       | length x /= 2     = error "Invalid argument for let"
                                    | otherwise         = evalExpr ram (List ([transformLetToLambda] ++ parameters))
                                            where
                                                transformLetToLambda = Procedure (args, body)
                                                args        = case x !! 0 of
                                                    List z  -> case z !! 0 of
                                                        KeyWord a   -> List [KeyWord a]
                                                        _           -> List [case y of 
                                                            List w -> w !! 0
                                                            _       -> error ("error to eval argument in Let: " ++ (show x))
                                                            | y <- z]
                                                    _       -> error "Bad Argument into Let"
                                                --
                                                parameters  = case x !! 0 of
                                                    List z  -> case z !! 0 of
                                                        KeyWord a   -> [giveExpr $ evalExpr ram $ z !! 1]
                                                        _           -> [giveExpr $ evalExpr ram $ case y of 
                                                            List w      -> w !! 1
                                                            _      -> error "error to eval parameter in Let"
                                                            | y <- z]
                                                    _       -> error "Bad Argument into Let"
                                                body        = x !! 1
--
evalExpr ram expr@(List x)         = case evalExpr ram $ head $ x of
    (new, Procedure (List args, body))    -> (new, result)
                                                where
                                                    y = tail x
                                                    defineArgs = [transformExprToMemory (List [(args !! i), giveExpr $ evalExpr new $ y !! i]) | i <- take (length args) [0,1..] ]
                                                    (fresh, result) = evalExpr (ram ++ defineArgs) body
    (new, KeyWord ('#':y))                -> (new, expr)
    -- Todo: Exhaustive case
    (new, z)                                -> (fresh, result)
                                                    where
                                                        y = tail x
                                                        (fresh, result) = evalExpr new z
    _                                   -> error "Can not evaluate list"
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
evalExpr ram x                      = error ("Impossible to evaluate expression " ++ show x)


evalFile :: [Expr] -> AccessMemory -> AccessMemory
evalFile [] x       = x
evalFile (x:xs) mem = evalFile xs (giveAccessMemory (evalExpr mem x)) 
{-
Eval a lisp file and put it to AccesMemory
-}
evalLisp :: String -> AccessMemory
evalLisp [] = []
evalLisp file = evalFile (parseExpr $ stringToToken $ file) []

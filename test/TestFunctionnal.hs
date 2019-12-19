module TestFunctionnal (testFunctionnal) where

import Test.QuickCheck
import Eval
import Tokenize
import Parser
import Control.Exception

testFunctionnal :: IO()
testFunctionnal = do
    -- Arithmetic Tests
    quickCheck(launch "(+ 4 4)"                 == "8")
    quickCheck(launch "(* 4 4)"                 == "16")
    quickCheck(launch "(- 4 4)"                 == "0")
    quickCheck(launch "(div 4 4)"               == "1")
    quickCheck(launch "(- 4)"                   == "-4")
    quickCheck(launch "(mod 15 4)"              == "3")
    quickCheck(launch "(< 4 3)"                 == "#f")
    quickCheck(launch "(< 3 4)"                 == "#t")
    quickCheck(launch "(- (+ 4 4))"             == "-8")
    quickCheck(launch "(mod (+ 2 4) (- 4 1))"   == "0")
    -- Quote Tests
    quickCheck(launch "(quote toto)"            == "toto")
    quickCheck(launch "(quote (+ 1 2))"         == "(+ 1 2)")
    quickCheck(launch "'toto"                   == "toto")
    quickCheck(launch "'(+ 1 2)"                == "(+ 1 2)")
    -- Cons Tests
    quickCheck(launch "(cons 1 2)"                      == "(1 . 2)")
    quickCheck(launch "(cons 1 (cons 2(cons 3 '())))"   == "(1 2 3)")
    quickCheck(launch "(cons 1 (cons 3 '()))"           == "(1 3)")
    quickCheck(launch "(cons '() '())"                  == "(())")
    quickCheck(launch "(cons '() 1)"                    == "(() . 1)")
    quickCheck(launch "(cons '(2 3) '(2 1))"            == "((2 3) 2 1)")
    -- Car Tests
    quickCheck(launch "(car (cons '() 1))"              == "()")
    quickCheck(launch "(car (cons 1 2))"                == "1")
    quickCheck(launch "(car '(1 2 3 4))"                == "1")
    -- Cdr Tests
    quickCheck(launch "(cdr (cons '() 1))"              == "1")
    quickCheck(launch "(cdr (cons 1 2))"                == "2")
    quickCheck(launch "(cdr '(1 2 3 4))"                == "(2 3 4)")
        where
            launch :: String -> String
            launch x = displayExpr $ evalExpr (AccessMemory { access = []}) ((parseExpr $ stringToToken $ x) !! 0)

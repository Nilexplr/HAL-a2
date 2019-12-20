module TestFunctionnal (testFunctionnal) where

import Test.QuickCheck
import Eval
import Tokenize
import Parser
import Control.Exception

testFunctionnal :: IO()
testFunctionnal = do
    -- Arithmetic Tests
    quickCheck(launch "(+ 4 4)"                         == "8")
    quickCheck(launch "(* 4 4)"                         == "16")
    quickCheck(launch "(- 4 4)"                         == "0")
    quickCheck(launch "(div 4 4)"                       == "1")
    quickCheck(launch "(- 4)"                           == "-4")
    quickCheck(launch "(mod 15 4)"                      == "3")
    quickCheck(launch "(< 4 3)"                         == "#f")
    quickCheck(launch "(< 3 4)"                         == "#t")
    quickCheck(launch "(- (+ 4 4))"                     == "-8")
    quickCheck(launch "(mod (+ 2 4) (- 4 1))"           == "0")
    -- Quote Tests
    quickCheck(launch "(quote toto)"                    == "toto")
    quickCheck(launch "(quote (+ 1 2))"                 == "(+ 1 2)")
    quickCheck(launch "'toto"                           == "toto")
    quickCheck(launch "'(+ 1 2)"                        == "(+ 1 2)")
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
    -- Cond Tests
    quickCheck(launch "(cond (#f 1) (#t (+ 1 1)))"      == "2")
    quickCheck(launch "(cond ((eq? 'foo (car '(foo bar))) 'here) ((eq? 1 2) 'there) (#t 'nope))"      == "here")
    -- List Tests
    quickCheck(launch "(list 1 2 3)"                    == "(1 2 3)")
    quickCheck(launch "(list (+ 1 1) (* 2 2))"          == "(2 4)")
    -- Lambda Tests
    quickCheck(launch "(lambda (a b) (+ a b))"          == "#<procedure>")
    quickCheck(launch "((lambda (a b) (+ a b)) 1 2)"    == "3")
    -- Define Tests
    quickCheck(launch "(define foo 42)"                         == "foo")
    quickCheck(launch "(define add (lambda (a b) (+ a b)))"     == "add")
    -- Let Tests
    quickCheck(launch "(let ((a 2) (b (+ 1 2))) (+ a b))"       == "5")
    -- Eq? Tests
    quickCheck(launch "(eq? 1 1)"                               == "#t")
    quickCheck(launch "(eq? (+ 1 1) 2)"                         == "#t")
    quickCheck(launch "(eq? 'foo (car '(foo bar)))(eq? '())"    == "#t")
    quickCheck(launch "(eq? 'foo 'bar)"                         == "#f")
    quickCheck(launch "(eq? '() '())"                           == "#t")
    -- Atom? Tests
    quickCheck(launch "(atom?  'foo)"                   == "#t")
    quickCheck(launch "(atom? '(1 2 3))"                == "#f")
    quickCheck(launch "(atom? '())"                     == "#t")
        where
            launch :: String -> String
            launch x = displayExpr $ giveExpr $ evalExpr [] ((parseExpr $ stringToToken $ x) !! 0)
            --

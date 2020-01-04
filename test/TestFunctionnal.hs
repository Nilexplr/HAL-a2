module TestFunctionnal (testFunctionnal, testMemory) where

import Test.QuickCheck
import Eval
import Tokenize
import Parser
import Control.Exception
import Test.Hspec
import Prelude hiding (catch)

testFunctionnal :: Spec
testFunctionnal = describe "Tests functionnel" $ do
    -- Arithmetic Tests
    it "Basic Addition" $
        launch "(+ 4 4)"                `shouldBe` "8"
    it "Basic Multiplication" $
        launch "(* 4 4)"                `shouldBe` "16"
    it "Basic soustraction" $
        launch "(- 4 4)"                `shouldBe` "0"
    it "Basic division" $
        launch "(div 4 4)"              `shouldBe` "1"
    it "Negative number" $
        launch "(- 4)"                  `shouldBe` "-4"
    it "Basic modulo" $
        launch "(mod 15 4)"             `shouldBe` "3"
    it "Inférieur à" $
        launch "(< 4 3)"                `shouldBe` "#f"
    it "False inférieur à" $
        launch "(< 3 4)"                `shouldBe` "#t"
    it "Complex calcul " $
        launch "(- (+ 4 4))"            `shouldBe` "-8"
    it "Complex modulo" $
        launch "(mod (+ 2 4) (- 4 1))"  `shouldBe` "0"
    -- Quote Tests
    it "Basic quote" $
        launch "(quote toto)"            `shouldBe` "toto"
    it "Quote an expression" $
        launch "(quote (+ 1 2))"         `shouldBe` "(+ 1 2)"
    it "sugar quote" $
        launch "'toto"                   `shouldBe` "toto"
    it "sugar quote expression" $
        launch "'(+ 1 2)"                `shouldBe` "(+ 1 2)"
    -- Cons Tests
    it "Cons 2 number" $
        launch "(cons 1 2)"                      `shouldBe` "(1 . 2)"
    it "Several cons" $
        launch "(cons 1 (cons 2(cons 3 '())))"   `shouldBe` "(1 2 3)"
    it "Double cons" $
        launch "(cons 1 (cons 3 '()))"           `shouldBe` "(1 3)"
    it "Cons empty lists" $
        launch "(cons '() '())"                  `shouldBe` "(())"
    it "Cons emptu list with atom" $
        launch "(cons '() 1)"                    `shouldBe` "(() . 1)"
    it "cons Two list" $
        launch "(cons '(2 3) '(2 1))"            `shouldBe` "((2 3) 2 1)"
    -- Car Tests
    it "Car return empty list" $
        launch "(car (cons '() 1))"              `shouldBe` "()"
    it "Basic car" $
        launch "(car (cons 1 2))"                `shouldBe` "1"
    it "car a sugar quote list" $
        launch "(car '(1 2 3 4))"                `shouldBe` "1"
    -- Cdr Tests
    it "Basic cdr" $
        launch "(cdr (cons '() 1))"              `shouldBe` "1"
    it "Basic cdr" $
        launch "(cdr (cons 1 2))"                `shouldBe` "2"
    it "Basic cdr" $
        launch "(cdr '(1 2 3 4))"                `shouldBe` "(2 3 4)"
    -- Cond Tests
    it "Basic cond" $
        launch "(cond (#f 1) (#t (+ 1 1)))"      `shouldBe` "2"
    it "complex Cond " $
        launch "(cond ((eq? 'foo (car '(foo bar))) 'here) ((eq? 1 2) 'there) (#t 'nope))"      `shouldBe` "here"
    -- List Tests
    it "Basic List" $
        launch "(list 1 2 3)"                    `shouldBe` "(1 2 3)"
    it "List with expression" $
        launch "(list (+ 1 1) (* 2 2))"          `shouldBe` "(2 4)"
    -- Lambda Tests
    it "Display lambda" $
        launch "(lambda (a b) (+ a b))"          `shouldBe` "#<procedure>"
    it "Basic lambda" $
        launch "((lambda (a b) (+ a b)) 1 2)"    `shouldBe` "3"
    -- Define Tests
    it "Display Basic Define" $
        launch "(define foo 42)"                         `shouldBe` "foo"
    it "Display Basic define function" $
        launch "(define add (lambda (a b) (+ a b)))"     `shouldBe` "add"
    -- Let Tests
    it "Basic Let" $
        launch "(let ((a 2) (b (+ 1 2))) (+ a b))"       `shouldBe` "5"
    -- Eq? Tests
    it "Basic Eq" $
        launch "(eq? 1 1)"                               `shouldBe` "#t"
    it "Basic Eq with expression" $
        launch "(eq? (+ 1 1) 2)"                         `shouldBe` "#t"
    it "Complex ex" $
        launch "(eq? 'foo (car '(foo bar)))(eq? '())"    `shouldBe` "#t"
    it "Basic eq String" $
        launch "(eq? 'foo 'bar)"                         `shouldBe` "#f"
    it "Basic eq empty list" $
        launch "(eq? '() '())"                           `shouldBe` "#t"
    -- Atom? Tests
    it "Basica atom string" $
        launch "(atom?  'foo)"                   `shouldBe` "#t"
    it "Basic atom list" $
        launch "(atom? '(1 2 3))"                `shouldBe` "#f"
    it "Basic empty list" $
        launch "(atom? '())"                     `shouldBe` "#t"
        where
            launch :: String -> String
            launch x = displayExpr $ giveExpr $ evalExpr [] ((parseExpr $ stringToToken $ x) !! 0)
            --

testMemory :: String -> Spec
testMemory into = describe "Tests memory functionnel" $ do
    it "Factoriel file test" $
        launch "(fact 2)" (evalLisp into)       `shouldBe` "2"
    it "Fibonnacci file test" $
        launch "(fib 10)" (evalLisp into)       `shouldBe` "55"
    it "sort file test" $
        launch "(merge-sort '(39 16 22 24 17 29 18 26 27 3 34 25 10 6 7 12 8 30 2 21 13 36 14 3832 41 40 4 35 19 5 33 23 9 15 31 28 20 42 37 11 1))" (evalLisp into)       `shouldBe` "(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 2324 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42)"
    where
        launch :: String -> AccessMemory -> String
        launch x mem = displayExpr $ giveExpr $ evalExpr mem ((parseExpr $ stringToToken $ x) !! 0)
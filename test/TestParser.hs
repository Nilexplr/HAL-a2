module TestParser (testParserBasic) where

import Test.QuickCheck
import Parser
import Tokenize
import Control.Exception

{-
Test basics of parse function
-}

testParserBasic :: IO ()
testParserBasic = do
    quickCheck((case parseValue [TokenOpen,
                            TokenOp Plus,
                            Number 3,
                            Number 6,
                            TokenClose] of
                    Nothing -> Val (-84)
                    Just (x, y) -> x) == Calcul Plus [Val 3, Val 6])
    quickCheck((case parseValue [TokenOpen,
                            TokenOp Minus,
                            Number 8,
                            Number 12,
                            TokenClose] of
                    Nothing -> Val (-84)
                    Just (x, y) -> x) == Calcul Minus [Val 8, Val 12])
    --quickCheck((parseValue []) == Nothing)
            
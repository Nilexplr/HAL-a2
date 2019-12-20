module TestToken (testToken) where

import Test.QuickCheck
import Tokenize
import Control.Exception

testToken :: IO ()
testToken = do
    quickCheck((stringToToken "+-()/^*") == [ TokenOpen
                                            , TokenClose
                                            ])
    quickCheck(stringToToken "2" == [Number 2])
    quickCheck(stringToToken "toto" == [Word "toto"])

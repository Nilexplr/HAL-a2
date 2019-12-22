import Tokenize
import Test.QuickCheck
import TestToken
import TestFunctionnal
import Test.Hspec

main :: IO ()
main = do 
    memFile <- fmap concat $ mapM readFile $    [ "lisp/fact.lisp"
                                                , "lisp/fib.lisp"
                                                , "lisp/sort.lisp" 
                                                ]
    hspec $ do
        testFunctionnal
        testMemory memFile

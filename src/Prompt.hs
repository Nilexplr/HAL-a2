module Prompt
    ( launchPrompt
    , displayEval
    )
    where

import System.IO
import Text.Printf
import Prelude hiding (catch)
import Control.Exception
import System.Exit
import Data.Char
import Control.Monad

import Parser
import Tokenize
import Eval

handler :: SomeException -> IO ()
handler ex = putStrLn $ "*** ERROR : " ++ show ex

displayEval :: AccessMemory -> String -> IO()
displayEval ram files = do
    catch (putStrLn $ displayExpr $ giveExpr $ evalExpr ram $ expr) handler
        where 
            expr   = last $ parseExpr $ stringToToken $ files

launchPrompt :: AccessMemory -> IO()
launchPrompt ram = do
        putStr "> " >> hFlush stdout
        out <- getLine
        let (new, expr) = (evalExpr ram ((parseExpr $ stringToToken $ out) !! 0))
        catch (putStrLn $ displayExpr $ expr) handler
        launchPrompt new
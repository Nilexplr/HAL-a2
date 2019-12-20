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

displayEval :: AccessMemory -> IO()
displayEval _ = putStrLn "In progress"--catch (putStrLn $ evalExpr $ (parseExpr $ stringToToken $ out) !! 0) handler

launchPrompt :: AccessMemory -> IO()
launchPrompt ram = forever $ do
        putStr "> " >> hFlush stdout
        out <- getLine
        let (new, expr) = (evalExpr ram ((parseExpr $ stringToToken $ out) !! 0))
        catch (putStrLn $ displayExpr $ expr) handler
        launchPrompt new
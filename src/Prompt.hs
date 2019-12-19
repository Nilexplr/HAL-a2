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
launchPrompt x = forever $ do
        putStr "> " >> hFlush stdout
        out <- getLine
        catch (putStrLn $ displayExpr $ (evalExpr x ((parseExpr $ stringToToken $ out) !! 0))) handler
                
module Prompt
    ( launchPrompt
    , displayEval
    )
    where

import Tokenize
import System.IO
import Text.Printf
import Prelude hiding (catch)
import Control.Exception
import System.Exit
import Data.Char
import Control.Monad
import Eval

displayEval :: AccessMemory -> IO()
displayEval _ = putStr "In Progress\n"


launchPrompt :: AccessMemory -> IO()
launchPrompt _ = forever $ do
        putStr "> " >> hFlush stdout
        out <- getLine
        catch (putStrLn $ out) handler -- eval function should be put here
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "*** ERROR : " ++ show ex

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

displayEval :: [String] -> IO()
displayEval [] = return ()

launchPrompt :: IO()
launchPrompt = forever $ do
        putStr "> " >> hFlush stdout
        out <- getLine
        catch (putStrLn $ out) handler -- eval function should be put here
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "*** ERROR : " ++ show ex

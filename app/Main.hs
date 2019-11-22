module Main where

--import Argument
import Prompt
import System.Exit
import System.Environment
import System.IO
import Text.Printf
import Prelude hiding (catch)
import Control.Exception
import System.Exit

onAbort e = do
    let x = show (e :: SomeException)
    putStrLn $ "\nExit"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> handle onAbort launchPrompt

-- main :: IO ()
-- main = do
--     argv <- getArgs
--     args <- handleArgument argv
--     case args of
--         Right   (opt)       -> do
--                 printCluster (imgCompressor (parseFile c, nbColors opt, convergenceLimit opt) [])
--         Left    (Invalid)   -> exitWith $ ExitFailure 84
--         _                   -> exitWith ExitSuccess

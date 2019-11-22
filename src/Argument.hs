module Argument
    ( ArgumentType(..)
    , Options(..)
    , handleArgument
    ) where

{-
Options data declaration
-}
data Options = Options
    { pathFile          :: [String]
    , interactive       :: Bool
    } deriving Show

{-
Return a default Options data
-}
startOption :: Options
startOption = Options
    { pathFile          = []
    , interactive       = True
    }

{--
Declaration of the Flag datatype used for GetOpt
--}
data ArgumentType =     Invalid
                    |   Helper
                    |   Version
                    |   Other
    deriving (Show, Enum)

{-
Main function of the argument handling
-}
handleArgument :: [String] -> IO (Either ArgumentType Options)
handleArgument args = case parseArgument args of
                    Right   t           -> do return $ Right t
                    Left    (Helper)    -> do programUsage ; return $ Left Helper
                    Left    (Version)   -> do programVersion ; return $ Left Version
                    Left    (_)         -> do programInvalidArgs ; return $ Left Invalid

{-
Return the parsed argument
-}
parseArgument :: [String] -> Either ArgumentType Options
parseArgument []      = Right Options { pathFile = [], interactive = True }
parseArgument ["-i"]      = Right Options { pathFile = [], interactive = True }
parseArgument ["--help"]    = Left  Helper
parseArgument ["--version"] = Left  Version
-- TODO : handling file
-- parseArgument x@[f:fs]      = Right Options { pathFile = x, interactive = True }
parseArgument _             = Left  Invalid

{-
Display the usage
-}
programUsage :: IO ()
programUsage = do putStrLn "./hal [FILE.lisp] [-i]"


{-
Display the program's version
-}
programVersion :: IO ()
programVersion = do putStrLn "HAL v0.0.1"

{-
Display invalid message error,
-}
programInvalidArgs :: IO ()
programInvalidArgs = do putStrLn "the given arguments are invalid, please use the --help option"
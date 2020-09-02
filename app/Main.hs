module Main where

import Parser
import System.Environment
import System.Exit
import Text.Parsec

main :: IO ()
main =
  do
    progname <- getProgName
    args <- getArgs
    checkArgs progname args
    input <- readFile (head args)
    let output = runParser parseRooProgram 0 "" input
    case output of
      Right ast -> print ast
      Left err -> do
        putStr "Parse error at "
        print err

checkArgs :: String -> [String] -> IO ()
checkArgs _ [filename] =
  return ()
checkArgs progname _ =
  do
    putStrLn ("Usage: " ++ progname ++ " filename\n\n")
    exitWith (ExitFailure 1)

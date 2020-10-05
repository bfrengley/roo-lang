-- |
-- Module: Main
-- Description: A Roo parser and pretty printer (and soon-to-be compiler).
-- Maintainer: Stewart Webb <sjwebb@student.unimelb.edu.au>
--             Ben Frengley <bfrengley@student.unimelb.edu.au>
--
-- This module defines the Roo compiler entrypoint.
module Main where

import AST (Program)
import qualified Data.Text as T
import qualified Data.Text.IO as I
import Parser (parseRooProgram)
import PrettyPrint (prettyPrint)
import SymbolTable
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)

-- | 'Opt' represents the command-line flags recognised by the compiler.
data Opt
  = -- | 'OPrint' corresponds to `-p`, and pretty prints a Roo program.
    OPrint
  | -- | 'OAST' corresponds to `-a`, and prints the Roo program's AST.
    OAST
  | -- | 'OCompare' corresponds to `-c`, and compares the pretty printed output to the result of
    -- parsing that output and pretty printing it again.
    OCompare

main :: IO ()
main =
  do
    progname <- getProgName
    args <- getArgs
    (file, opt) <- checkArgs progname args
    input <- readFile file
    let output = parseRooProgram file input
    case output of
      Right ast -> runWithOpts input ast opt
      Left err -> do
        putStr "Parse error at "
        print err
        exitWith (ExitFailure 2)

-- | 'checkArgs' tests the arguments provided to the program against the recognised arguments.
-- If it finds unrecognised arguments, it exits and prints a usage message.
checkArgs :: String -> [String] -> IO (String, Maybe Opt)
checkArgs _ ["-p", filename] =
  return (filename, Just OPrint)
checkArgs _ ["-a", filename] =
  return (filename, Just OAST)
checkArgs _ ["-c", filename] =
  return (filename, Just OCompare)
checkArgs _ [filename] =
  return (filename, Nothing)
checkArgs progname _ =
  do
    putStrLn $ "Usage: " ++ progname ++ " (-a|-p|-c) filename"
    putStrLn ""
    putStrLn "  Flags:"
    putStrLn "    -a: print the program's AST"
    putStrLn "    -p: pretty print the program"
    putStrLn "    -c: compare the pretty printed output of the program against the result of"
    putStrLn "        parsing the output and printing it again"
    exitWith (ExitFailure 1)

-- 'runWithOpts' executes the appropriate operation on the Roo program based on the provided
-- arguments.
runWithOpts :: String -> Program -> Maybe Opt -> IO ()
runWithOpts _ ast (Just OPrint) = I.putStr $ prettyPrint ast
runWithOpts _ ast (Just OAST) = print ast
runWithOpts _ ast (Just OCompare) =
  if isPrintParseIdempotent ast
    then putStrLn "OK."
    else exitWith (ExitFailure 2)
runWithOpts source ast Nothing = printSymbolTableErrors source ast

-- | 'isPrintParseIdempotent' checks if pretty printing a program retains the same parse by
-- comparing the pretty printed program to the result of parsing the pretty printed output and
-- printing that again. If the pretty printed output of the second round matches that of the first,
-- the programs are considered equivalent.
isPrintParseIdempotent :: Program -> Bool
isPrintParseIdempotent ast =
  let firstPrint = return $ prettyPrint ast
      secondPrint = prettyPrint <$> (firstPrint >>= (parseRooProgram "") . T.unpack)
   in firstPrint == secondPrint

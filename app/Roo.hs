-- |
-- Module: Main
-- Description: A Roo parser and pretty printer (and soon-to-be compiler).
-- Maintainer: Stewart Webb <sjwebb@student.unimelb.edu.au>
--             Ben Frengley <bfrengley@student.unimelb.edu.au>
--
-- This module defines the Roo compiler entrypoint.
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as I
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)

import AST (Program)
import Analysis (analyseProgram)
import CodeGen (generateCode)
import OzWriter (writeProgram)
import Parser (parseRooProgram)
import PrettyPrint (prettyPrint)
import Semantics (writeError)

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
    inputRooSource <- readFile file

    -- Try to parse the source code
    let parseResult = parseRooProgram file inputRooSource in
      case parseResult of
        -- Success - can proceed with a parsed Roo AST
        Right ast ->
          case opt of
            Just OPrint ->
              -- Output a pretty-printed version of the parsed AST
              I.putStr (prettyPrint ast)
            Just OAST ->
              -- Output the AST as a Haskell datastructure
              print ast
            Just OCompare ->
              if isPrintParseIdempotent ast
                then putStrLn "OK."
                else exitWith (ExitFailure 2)
            Nothing ->
              -- Treat the default option/behaviour as compilation

              -- Perform semantic analysis
              case (analyseProgram ast) of
                Right tables ->
                  -- no errors; generate code
                  let compiledProgram = generateCode ast tables
                  in
                    -- Output a formatted/textual representation of the Oz program to stdout
                    putStrLn . T.unpack $ writeProgram compiledProgram
                Left errors ->
                  -- semantic errors found, print them out
                  mapM_ (putStrLn . T.unpack . writeError (lines inputRooSource)) errors


        -- Parse error
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

-- | 'isPrintParseIdempotent' checks if pretty printing a program retains the same parse by
-- comparing the pretty printed program to the result of parsing the pretty printed output and
-- printing that again. If the pretty printed output of the second round matches that of the first,
-- the programs are considered equivalent.
isPrintParseIdempotent :: Program -> Bool
isPrintParseIdempotent ast =
  let firstPrint = return $ prettyPrint ast
      secondPrint = prettyPrint <$> (firstPrint >>= parseRooProgram "" . T.unpack)
   in firstPrint == secondPrint

module Main where

import AST (Program)
import qualified Data.Text as T
import qualified Data.Text.IO as I
import Parser (parseRooProgram)
import PrettyPrint (prettyPrint)
import System.Environment
import System.Exit
import Text.Pretty.Simple (pPrint)

data Opt = OPrint | OAST | OCompare

main :: IO ()
main =
  do
    progname <- getProgName
    args <- getArgs
    (file, opt) <- checkArgs progname args
    input <- readFile file
    let output = parseRooProgram input
    case output of
      Right ast ->
        case opt of
          Just OPrint -> I.putStrLn $ prettyPrint ast
          Just OAST -> pPrint ast
          Just OCompare ->
            if isPrintParseIdempotent ast
              then putStrLn "OK."
              else exitWith (ExitFailure 2)
          _ -> do
            putStrLn "Target code not yet implemented"
            exitWith (ExitFailure 1)
      Left err -> do
        putStr "Parse error at "
        print err

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
    putStrLn "    -c: compare the AST of the program with the AST after pretty printing and parsing again"
    exitWith (ExitFailure 1)

isPrintParseIdempotent :: Program -> Bool
isPrintParseIdempotent ast =
  let printParse = parseRooProgram . T.unpack . prettyPrint
      oneRound = printParse ast
      twoRounds = oneRound >>= printParse
   in oneRound == twoRounds

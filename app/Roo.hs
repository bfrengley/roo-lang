module Main where

import qualified Data.Text.IO as I
import Parser (parseRooProgram)
import PrettyPrint (prettyPrint)
import System.Environment
import System.Exit
import Text.Pretty.Simple (pPrint)

data Opt = OPrint | OAST

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
          _ -> do
            putStrLn "Target code not yet implemented"
            exitWith (ExitFailure 2)
      Left err -> do
        putStr "Parse error at "
        print err

checkArgs :: String -> [String] -> IO (String, Maybe Opt)
checkArgs _ ["-p", filename] =
  return (filename, Just OPrint)
checkArgs _ ["-a", filename] =
  return (filename, Just OAST)
checkArgs _ [filename] =
  return (filename, Nothing)
checkArgs progname _ =
  do
    putStrLn ("Usage: " ++ progname ++ " (-a|-p) filename")
    exitWith (ExitFailure 1)

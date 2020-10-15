module Semantics
  ( SemanticError (..),
    SemanticState,
    addError,
    writeError,
  )
where

import AST
import Control.Monad.State
import Data.List (intercalate)
import Text.Parsec (SourcePos, sourceColumn, sourceLine, sourceName)

data SemanticError
  = Redefinition Ident Ident
  | Unknown Ident
  | InvalidArrayType Ident Ident
  | MissingMain
  deriving (Show)

type SemanticState a = State [SemanticError] a

addError :: SemanticError -> SemanticState ()
addError err = modify (err :)

writeError :: [String] -> SemanticError -> String
writeError source (Redefinition (Ident pos name) (Ident pos' _)) =
  unlines
    [ writePos pos ++ ": error: `" ++ name ++ "` redefined",
      writeContext source pos,
      writePos pos' ++ ": note: previously defined here",
      writeContext source pos'
    ]
writeError source (Unknown (Ident pos name)) =
  unlines
    [ writePos pos ++ ": error: unknown type `" ++ name ++ "`",
      writeContext source pos
    ]
writeError source (InvalidArrayType (Ident pos name) (Ident pos' _)) =
  unlines
    [ writePos pos ++ ": error: invalid underlying type `" ++ name ++ "` for an array type",
      writeContext source pos,
      writePos pos' ++ ": note: type defined here",
      writeContext source pos'
    ]
writeError _ MissingMain =
  unlines ["error: no `main` procedure found"] -- position at end of source

writeContext :: [String] -> SourcePos -> String
writeContext source pos =
  source !! (sourceLine pos - 1) ++ "\n"
    ++ replicate (sourceColumn pos - 1) ' '
    ++ "^"

writePos :: SourcePos -> String
writePos pos = intercalate ":" [sourceName pos, show $ sourceLine pos, show $ sourceColumn pos]

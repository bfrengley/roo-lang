{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Semantics
-- Description: This module defines common types for recording semantic errors in Roo programs.
-- Maintainer: Stewart Webb <sjwebb@student.unimelb.edu.au>
--             Ben Frengley <bfrengley@student.unimelb.edu.au>
--
-- The types and functions defined in this module are the common types used for checking the
-- semantics of a Roo program and for displaying errors.
module Semantics where

import AST
import Control.Monad.State.Strict (State)
import Control.Monad.Writer.Strict (MonadWriter (tell), WriterT)
import Data.Text (Text)
import qualified Data.Text as T
import PrettyPrint (pPrintBuiltinType, printBinOp, showT)
import Text.Parsec (SourcePos, sourceColumn, sourceLine, sourceName)
import Util (Equivalent (..))

-- | The type of a symbol. This is defined here rather than with the symbol table to avoid a
-- circular reference (the errors don't belong to the symbol table so they're defined here, and
-- many of them refer to symbol types so the types can't be defined in the symbol table).
data SymbolType
  = AliasT Text ProcParamPassMode
  | BuiltinT BuiltinType ProcParamPassMode
  | StringT
  | -- | UnknownT is the bottom type, which is the type of invalid/unknown variables and is loosely
    -- equal to all other types to allow for nicer error messages.
    UnknownT
  deriving (Show, Eq)

-- Loose equality/equivalence for symbol types, which ignores modes and allows the bottom type to
-- match anything. For more info see 'Equivalent'.
instance Equivalent SymbolType where
  (AliasT name _) =%= (AliasT name' _) = name == name'
  (BuiltinT t _) =%= (BuiltinT t' _) = t == t'
  UnknownT =%= _ = True
  _ =%= UnknownT = True
  t1 =%= t2 = t1 == t2

-- | SemanticState is a state wrapper which supports recording errors using 'addError'. This keeps
-- the errors out of the state and return types and allows the use of different types of states
-- with the same error reporting mechanism, but it does make the monad handling a little trickier.
type SemanticState s = WriterT [SemanticError] (State s)

-- | Record an error inside a monad which allows writing.
addError :: MonadWriter [SemanticError] m => SemanticError -> m ()
addError err = tell [err]

-- Semantic errors. Uhh
-- Find the 'writeError'' entry for more detail on each error
data SemanticError
  = Redefinition Ident Ident
  | UnknownType Ident
  | UnknownVar Ident
  | UnknownProcedure Ident
  | ArgumentCountMismatch SourcePos Ident Int Int
  | ArgumentTypeMismatch SourcePos Ident SymbolType SymbolType
  | InvalidReadType SourcePos SymbolType [SymbolType]
  | InvalidArrayType Ident Ident
  | InvalidUnaryType SourcePos UnaryOp SymbolType SymbolType
  | InvalidBinaryType SourcePos BinaryOp SymbolType [SymbolType]
  | InvalidConditionType SourcePos SymbolType SymbolType
  | BinaryTypeMismatch SourcePos BinaryOp SymbolType SymbolType
  | InvalidAssign SourcePos Ident SymbolType SymbolType
  | UnexpectedIndex SourcePos Ident SymbolType (Maybe SourcePos)
  | UnexpectedField SourcePos Ident SymbolType (Maybe SourcePos)
  | UnknownField Ident Ident SymbolType Ident
  | AliasLoadInValueMode Ident Ident SymbolType
  | AliasWrite SourcePos SymbolType
  | InvalidIndexType SourcePos SymbolType SymbolType
  | IndexOutOfBounds SourcePos Integer Integer Ident Ident
  | MissingMain
  | MainArity SourcePos Int
  deriving (Show)

-- | Write an error as a nice human readable, with context from the source code.
writeError :: [String] -> SemanticError -> Text
writeError source = T.unlines . writeError' source

-- | This writes error messages nicely, but it sure isn't written nicely. Good thing it's actually
-- pretty simple, there are just lots of errors.
writeError' :: [String] -> SemanticError -> [Text]
writeError' source (Redefinition (Ident pos name) (Ident pos' _)) =
  [ errorStart pos <> ticks (T.pack name) <> " redefined",
    writeContext source pos,
    noteStart pos' <> "previously defined here",
    writeContext source pos'
  ]
writeError' source (UnknownType (Ident pos name)) =
  [ errorStart pos <> "undeclared type " <> ticks (T.pack name),
    writeContext source pos
  ]
writeError' source (UnknownVar (Ident pos name)) =
  [ errorStart pos <> "undeclared variable " <> ticks (T.pack name),
    writeContext source pos
  ]
writeError' source (UnknownProcedure (Ident pos name)) =
  [ errorStart pos <> "undeclared procedure " <> ticks (T.pack name),
    writeContext source pos
  ]
writeError' source (InvalidArrayType (Ident pos name) (Ident pos' _)) =
  [ errorStart pos
      <> "invalid underlying type "
      <> ticks (T.pack name)
      <> " for an array type",
    writeContext source pos,
    noteStart pos' <> "type defined here",
    writeContext source pos'
  ]
writeError' source (InvalidUnaryType pos op found expected) =
  [ errorStart pos <> "invalid operand type "
      <> ticks (printLocalType NoPrintMode found)
      <> " for operator "
      <> ticks (writeUnaryOp op)
      <> " (expected "
      <> ticks (printLocalType NoPrintMode expected)
      <> ")",
    writeContext source pos
  ]
writeError' source (InvalidBinaryType pos op found expected) =
  [ errorStart pos <> "invalid operand type "
      <> ticks (printLocalType NoPrintMode found)
      <> " for operator "
      <> ticks (printBinOp op)
      <> " (expected "
      <> writeExpectedTypes expected
      <> ")",
    writeContext source pos
  ]
writeError' source (BinaryTypeMismatch pos op left right) =
  [ errorStart pos <> "cannot apply operator "
      <> ticks (printBinOp op)
      <> " to operands of mismatched types "
      <> ticks (printLocalType NoPrintMode left)
      <> " and "
      <> ticks (printLocalType NoPrintMode right),
    writeContext source pos
  ]
writeError' source (InvalidReadType pos actual expected) =
  [ errorStart pos <> "cannot read value of type " <> ticks (printLocalType NoPrintMode actual)
      <> " (expected "
      <> writeExpectedTypes expected
      <> ")",
    writeContext source pos
  ]
writeError' source (InvalidAssign pos (Ident pos' _) varT exprT) =
  [ errorStart pos <> "cannot assign value of type "
      <> ticks (printLocalType PrintMode exprT)
      <> " to variable of type "
      <> ticks (printLocalType PrintMode varT),
    writeContext source pos,
    noteStart pos' <> "variable declared here:",
    writeContext source pos'
  ]
writeError' source (UnexpectedIndex pos (Ident pos' _) varT typeDeclPos) =
  let typeDeclNote = case typeDeclPos of
        (Just tPos) ->
          [ noteStart tPos <> "type declared here:",
            writeContext source tPos
          ]
        Nothing -> []
   in [ errorStart pos <> "unexpected index expression on non-array type "
          <> ticks (printLocalType NoPrintMode varT),
        writeContext source pos,
        noteStart pos' <> "variable declared here:",
        writeContext source pos'
      ]
        ++ typeDeclNote
writeError' source (InvalidIndexType pos actualT expectedT) =
  [ errorStart pos <> "invalid type " <> ticks (printLocalType NoPrintMode actualT)
      <> " for index expression (expected "
      <> ticks (printLocalType NoPrintMode expectedT)
      <> ")",
    writeContext source pos
  ]
writeError' source (IndexOutOfBounds pos idx size (Ident varPos _) (Ident typePos typeName)) =
  [ errorStart pos <> "index out-of-bounds (array type " <> ticks (T.pack typeName)
      <> " has length "
      <> showT size
      <> ", but the index expression evaluates to "
      <> showT idx
      <> ")",
    writeContext source pos,
    noteStart varPos <> "variable declared here:",
    writeContext source varPos,
    noteStart typePos <> "type declared here:",
    writeContext source typePos
  ]
writeError' source (UnexpectedField pos (Ident pos' _) varT typeDeclPos) =
  let typeDeclNote = case typeDeclPos of
        (Just tPos) ->
          [ noteStart tPos <> "type declared here:",
            writeContext source tPos
          ]
        Nothing -> []
   in [ errorStart pos <> "unexpected field access on non-record type "
          <> ticks (printLocalType NoPrintMode varT),
        writeContext source pos,
        noteStart pos' <> "variable declared here:",
        writeContext source pos'
      ]
        ++ typeDeclNote
writeError' source (UnknownField (Ident pos field) (Ident varPos _) baseT (Ident typePos _)) =
  [ errorStart pos <> "unknown field " <> ticks (T.pack field) <> " on type "
      <> ticks (printLocalType NoPrintMode baseT),
    writeContext source pos,
    noteStart varPos <> "variable declared here:",
    writeContext source varPos,
    noteStart typePos <> "type declared here:",
    writeContext source typePos
  ]
writeError' source (AliasLoadInValueMode (Ident pos _) (Ident varPos _) t) =
  [ errorStart pos <> "cannot load alias type " <> ticks (printLocalType NoPrintMode t)
      <> " in value mode",
    writeContext source pos,
    noteStart varPos <> "variable declared here:",
    writeContext source varPos
  ]
writeError' source (AliasWrite pos t) =
  [ errorStart pos <> "cannot write alias type " <> ticks (printLocalType NoPrintMode t),
    writeContext source pos
  ]
writeError' source (ArgumentCountMismatch callPos (Ident procPos procId) found expected) =
  [ errorStart callPos <> "incorrect number of arguments provided for call to procedure "
      <> ticks (T.pack procId)
      <> " (expected "
      <> showT expected
      <> ", found "
      <> showT found
      <> ")",
    writeContext source callPos,
    noteStart procPos <> "procedure declared here:",
    writeContext source procPos
  ]
writeError' source (ArgumentTypeMismatch argPos (Ident paramPos _) found expected) =
  [ errorStart argPos <> "invalid argument type "
      <> ticks (printLocalType PrintMode found)
      <> " (expected "
      <> ticks (printLocalType PrintMode expected)
      <> ")",
    writeContext source argPos,
    noteStart paramPos <> "parameter declared here:",
    writeContext source paramPos
  ]
writeError' source (InvalidConditionType pos actualT expectedT) =
  [ errorStart pos <> "invalid condition type " <> ticks (printLocalType NoPrintMode actualT)
      <> " (expected "
      <> ticks (printLocalType NoPrintMode expectedT)
      <> ")",
    writeContext source pos
  ]
writeError' _ MissingMain =
  ["error: no `main` procedure found"] -- position at end of source
writeError' source (MainArity pos arity) =
  [ errorStart pos <> "`main` function has arity " <> showT arity <> " (expected 0)",
    writeContext source pos
  ]

-- | Write the line of source code the position belongs to with a pointer to the specific column.
-- The position was derived from parsing the same source used here, so the '!!' is safe.
writeContext :: [String] -> SourcePos -> Text
writeContext source pos =
  T.pack (source !! (sourceLine pos - 1)) <> "\n"
    <> T.replicate (sourceColumn pos - 1) " "
    <> "^"

-- | Write a list of expected types for errors which support multiple (like binary operand type
-- errors).
writeExpectedTypes :: [SymbolType] -> Text
writeExpectedTypes [t] = ticks (printLocalType NoPrintMode t)
writeExpectedTypes expected =
  let types = map (ticks . printLocalType NoPrintMode) expected
   in "one of " <> T.intercalate ", " types

-- | Whether to print the mode of a type or not.
data PrintTypeOpt = PrintMode | NoPrintMode deriving (Eq)

-- | Print a local type with or without its mode, as specified.
printLocalType :: PrintTypeOpt -> SymbolType -> Text
printLocalType _ StringT = "string"
printLocalType _ UnknownT = "unknown" -- ??? does this ever come up?
printLocalType NoPrintMode (AliasT name _) = name
printLocalType PrintMode (AliasT name mode) = T.unwords [name, printPassMode mode]
printLocalType NoPrintMode (BuiltinT t _) = pPrintBuiltinType t
printLocalType PrintMode (BuiltinT t mode) = T.unwords [pPrintBuiltinType t, printPassMode mode]

printPassMode :: ProcParamPassMode -> Text
printPassMode PassByRef = "ref"
printPassMode PassByVal = "val"

-- | Write the position into a consistent, short format for starting error messages.
writePos :: SourcePos -> Text
writePos pos =
  let parts = [sourceName pos, show $ sourceLine pos, show $ sourceColumn pos]
   in T.intercalate ":" $ map T.pack parts

writeUnaryOp :: UnaryOp -> Text
writeUnaryOp OpNot = "not"
writeUnaryOp OpNeg = "-"

ticks :: Text -> Text
ticks inner = "`" <> inner <> "`"

errorStart :: SourcePos -> Text
errorStart pos = writePos pos <> ": error: "

noteStart :: SourcePos -> Text
noteStart pos = writePos pos <> ": note: "

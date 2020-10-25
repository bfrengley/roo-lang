{-# LANGUAGE OverloadedStrings #-}

module Semantics where

import AST
import Control.Monad.State.Strict
import Data.Text (Text)
import qualified Data.Text as T
import PrettyPrint (pPrintBuiltinType, printBinOp)
import Text.Parsec (SourcePos, sourceColumn, sourceLine, sourceName)

data LocalType
  = AliasT Text ProcParamPassMode
  | BuiltinT BuiltinType ProcParamPassMode
  | StringT
  | UnknownT
  deriving (Show, Eq)

-- loose equality between types (i.e., ignoring modes and allowing UnknownT to equal everything)
(=%=) :: LocalType -> LocalType -> Bool
(AliasT name _) =%= (AliasT name' _) = name == name'
(BuiltinT t _) =%= (BuiltinT t' _) = t == t'
UnknownT =%= _ = True
_ =%= UnknownT = True
t1 =%= t2 = t1 == t2

data SemanticError
  = Redefinition Ident Ident
  | UnknownType Ident
  | UnknownVar Ident
  | InvalidArrayType Ident Ident
  | InvalidUnaryType SourcePos UnaryOp LocalType LocalType
  | InvalidBinaryType SourcePos BinaryOp LocalType [LocalType]
  | BinaryTypeMismatch SourcePos BinaryOp LocalType LocalType
  | InvalidAssign SourcePos Ident LocalType LocalType
  | UnexpectedIndex SourcePos Ident LocalType (Maybe SourcePos)
  | MissingMain
  deriving (Show)

type SemanticState a = State [SemanticError] a

addError :: SemanticError -> SemanticState ()
addError err = modify (err :)

writeError :: [String] -> SemanticError -> Text
writeError source = T.unlines . writeError' source

writeError' :: [String] -> SemanticError -> [Text]
writeError' source (Redefinition (Ident pos name) (Ident pos' _)) =
  [ errorStart pos <> ticks (T.pack name) <> " redefined",
    writeContext source pos,
    noteStart pos' <> "previously defined here",
    writeContext source pos'
  ]
writeError' source (UnknownType (Ident pos name)) =
  [ errorStart pos <> "unknown type " <> ticks (T.pack name),
    writeContext source pos
  ]
writeError' source (UnknownVar (Ident pos name)) =
  [ errorStart pos <> "unknown variable " <> ticks (T.pack name),
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
      <> writeExpectedTypes NoPrintMode expected
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
writeError' source (InvalidAssign pos (Ident pos' _) varT exprT) =
  [ errorStart pos <> "cannot assign value of type "
      <> ticks (printLocalType PrintMode exprT)
      <> " to variable of type "
      <> ticks (printLocalType PrintMode varT),
    writeContext source pos,
    noteStart pos' <> "variable declared here:",
    writeContext source pos'
  ]
writeError' source (UnexpectedIndex pos (Ident pos' name) varT typeDeclPos) =
  let typeDeclNote =
        maybe
          []
          ( \tPos ->
              [ noteStart tPos <> "type declared here:",
                writeContext source tPos
              ]
          )
          typeDeclPos
   in [ errorStart pos <> "unexpected index expression for variable "
          <> ticks (T.pack name)
          <> " of non-array type "
          <> ticks (printLocalType NoPrintMode varT),
        writeContext source pos,
        noteStart pos' <> "variable declared here:",
        writeContext source pos'
      ]
        ++ typeDeclNote
writeError' _ MissingMain =
  ["error: no `main` procedure found"] -- position at end of source

writeContext :: [String] -> SourcePos -> Text
writeContext source pos =
  T.pack (source !! (sourceLine pos - 1)) <> "\n"
    <> T.replicate (sourceColumn pos - 1) " "
    <> "^"

writeExpectedTypes :: PrintTypeOpt -> [LocalType] -> Text
writeExpectedTypes opt [t] = ticks (printLocalType opt t)
writeExpectedTypes opt expected =
  let types = map (ticks . printLocalType opt) expected
   in "one of " <> T.intercalate ", " types

data PrintTypeOpt = PrintMode | NoPrintMode deriving (Eq)

printLocalType :: PrintTypeOpt -> LocalType -> Text
printLocalType _ StringT = "string"
printLocalType _ UnknownT = "unknown" -- ???
printLocalType NoPrintMode (AliasT name _) = name
printLocalType PrintMode (AliasT name mode) = T.unwords [name, printPassMode mode]
printLocalType NoPrintMode (BuiltinT t _) = pPrintBuiltinType t
printLocalType PrintMode (BuiltinT t mode) = T.unwords [pPrintBuiltinType t, printPassMode mode]

printPassMode :: ProcParamPassMode -> Text
printPassMode PassByRef = "reference"
printPassMode PassByVal = "value"

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

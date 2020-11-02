{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Semantics where

import AST
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Text (Text)
import qualified Data.Text as T
import PrettyPrint (pPrintBuiltinType, printBinOp, showT)
import Text.Parsec (SourcePos, sourceColumn, sourceLine, sourceName)
import Util (Equivalent (..))

data SymbolType
  = AliasT Text ProcParamPassMode
  | BuiltinT BuiltinType ProcParamPassMode
  | StringT
  | UnknownT
  deriving (Show, Eq)

instance Equivalent SymbolType where
  (AliasT name _) =%= (AliasT name' _) = name == name'
  (BuiltinT t _) =%= (BuiltinT t' _) = t == t'
  UnknownT =%= _ = True
  _ =%= UnknownT = True
  t1 =%= t2 = t1 == t2

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
  | BinaryTypeMismatch SourcePos BinaryOp SymbolType SymbolType
  | InvalidAssign SourcePos Ident SymbolType SymbolType
  | UnexpectedIndex SourcePos Ident SymbolType (Maybe SourcePos)
  | UnexpectedField SourcePos Ident SymbolType (Maybe SourcePos)
  | UnknownField Ident Ident SymbolType Ident
  | AliasLoadInValueMode Ident Ident SymbolType
  | AliasWrite SourcePos SymbolType
  | InvalidIndexType SourcePos SymbolType SymbolType
  | MissingMain
  | MainArity SourcePos Int
  deriving (Show)

type SemanticState s = WriterT [SemanticError] (State s)

addError :: MonadWriter [SemanticError] m => SemanticError -> m ()
addError err = tell [err]

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
writeError' source (InvalidReadType pos actual expected) =
  [ errorStart pos <> "cannot read value of type " <> ticks (printLocalType NoPrintMode actual)
      <> " (expected "
      <> writeExpectedTypes NoPrintMode expected
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
writeError' _ MissingMain =
  ["error: no `main` procedure found"] -- position at end of source
writeError' source (MainArity pos arity) =
  [ errorStart pos <> "`main` function has arity " <> showT arity <> " (expected 0)",
    writeContext source pos
  ]

writeContext :: [String] -> SourcePos -> Text
writeContext source pos =
  T.pack (source !! (sourceLine pos - 1)) <> "\n"
    <> T.replicate (sourceColumn pos - 1) " "
    <> "^"

writeExpectedTypes :: PrintTypeOpt -> [SymbolType] -> Text
writeExpectedTypes opt [t] = ticks (printLocalType opt t)
writeExpectedTypes opt expected =
  let types = map (ticks . printLocalType opt) expected
   in "one of " <> T.intercalate ", " types

data PrintTypeOpt = PrintMode | NoPrintMode deriving (Eq)

printLocalType :: PrintTypeOpt -> SymbolType -> Text
printLocalType _ StringT = "string"
printLocalType _ UnknownT = "unknown" -- ???
printLocalType _ (AliasT name _) = name
printLocalType NoPrintMode (BuiltinT t _) = pPrintBuiltinType t
printLocalType PrintMode (BuiltinT t mode) = T.unwords [pPrintBuiltinType t, printPassMode mode]

printPassMode :: ProcParamPassMode -> Text
printPassMode PassByRef = "ref"
printPassMode PassByVal = "val"

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

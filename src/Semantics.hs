{-# LANGUAGE OverloadedStrings #-}

module Semantics where

import AST
import Control.Monad.State
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
  | Unknown Ident
  | InvalidArrayType Ident Ident
  | InvalidUnaryType SourcePos UnaryOp LocalType LocalType
  | InvalidBinaryType SourcePos BinaryOp LocalType [LocalType]
  | BinaryTypeMismatch SourcePos BinaryOp LocalType LocalType
  | InvalidAssign SourcePos Ident LocalType LocalType
  | MissingMain
  deriving (Show)

type SemanticState a = State [SemanticError] a

addError :: SemanticError -> SemanticState ()
addError err = modify (err :)

writeError :: [String] -> SemanticError -> Text
writeError source (Redefinition (Ident pos name) (Ident pos' _)) =
  T.unlines
    [ writePos pos <> ": error: `" <> T.pack name <> "` redefined",
      writeContext source pos,
      writePos pos' <> ": note: previously defined here",
      writeContext source pos'
    ]
writeError source (Unknown (Ident pos name)) =
  T.unlines
    [ writePos pos <> ": error: unknown type `" <> T.pack name <> "`",
      writeContext source pos
    ]
writeError source (InvalidArrayType (Ident pos name) (Ident pos' _)) =
  T.unlines
    [ writePos pos <> ": error: invalid underlying type `" <> T.pack name <> "` for an array type",
      writeContext source pos,
      writePos pos' <> ": note: type defined here",
      writeContext source pos'
    ]
writeError source (InvalidUnaryType pos op found expected) =
  T.unlines
    [ writePos pos <> ": error: cannot apply operator `" <> writeUnaryOp op
        <> "` to operand of type "
        <> printLocalType NoPrintMode found
        <> " (expected "
        <> printLocalType NoPrintMode expected
        <> ")",
      writeContext source pos
    ]
writeError source (InvalidBinaryType pos op found expected) =
  T.unlines
    [ writePos pos <> ": error: cannot apply operator `" <> printBinOp op
        <> "` to operand of type "
        <> printLocalType NoPrintMode found
        <> " (expected "
        <> ( case expected of
               [t] -> printLocalType NoPrintMode t
               _ -> T.intercalate ", " (map (printLocalType NoPrintMode) expected)
           )
        <> ")",
      writeContext source pos
    ]
writeError source (BinaryTypeMismatch pos op left right) =
  T.unlines
    [ writePos pos <> ": error: cannot apply operator `" <> printBinOp op
        <> "` to operands of mismatched types "
        <> printLocalType NoPrintMode left
        <> " and "
        <> printLocalType NoPrintMode right,
      writeContext source pos
    ]
writeError source (InvalidAssign pos (Ident pos' _) varT exprT) =
  T.unlines
    [ writePos pos <> ": error: cannot assign value of type "
        <> printLocalType PrintMode exprT
        <> " to variable of type "
        <> printLocalType PrintMode varT,
      writeContext source pos,
      writePos pos' <> ": note: variable declared here:",
      writeContext source pos'
    ]
writeError _ MissingMain =
  T.unlines ["error: no `main` procedure found"] -- position at end of source

writeContext :: [String] -> SourcePos -> Text
writeContext source pos =
  T.pack (source !! (sourceLine pos - 1)) <> "\n"
    <> T.replicate (sourceColumn pos - 1) " "
    <> "^"

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
writePos pos = T.intercalate ":" $ map T.pack [sourceName pos, show $ sourceLine pos, show $ sourceColumn pos]

writeUnaryOp :: UnaryOp -> Text
writeUnaryOp OpNot = "not"
writeUnaryOp OpNeg = "-"

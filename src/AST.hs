-- |
-- Module: AST
-- Description: Data types which encode the abstract syntax tree of a valid Roo program.
-- Maintainer: Stewart Webb <sjwebb@student.unimelb.edu.au>
--             Ben Frengley <bfrengley@student.unimelb.edu.au>
--
-- A Roo program consists of: any number of record type definitions, any number of array type
-- definitions, and at least one procedure definition. The AST of a Roo program is represented
-- by the 'Program' type.
--
-- This module is defined in a bottom-up order, beginning with the smaller components of the grammar
-- and building up to a full Roo program.
module AST where

import Text.Parsec (SourcePos)

-- | 'Ident' encodes a Roo identifier.
data Ident = Ident SourcePos String deriving (Eq, Show)

-- | 'UnaryOp' encodes unary operators.
data UnaryOp
  = -- | Unary minus, which negates a numerical argument (e.g., `-1`).
    OpNeg
  | -- | Unary `not`, which negates a boolean argument (e.g., `not true`).
    OpNot
  deriving (Eq, Show)

-- | 'BinaryOp' encodes binary operators, including arithmetic, relational, and boolean operators.
--
-- The full operator table is as follows (precedence increases from top to bottom):
--
-- +-----------+---------------------------------+
-- | Operator  | Description                     |
-- +-----------+---------------------------------+
-- | `or`      | Logical disjunction             |
-- +-----------+---------------------------------+
-- | `and`     | Logical conjunction             |
-- +-----------+---------------------------------+
-- | `not`     | Logical negation                |
-- +-----------+---------------------------------+
-- | `=`, `!=`,| Relational operators. These     |
-- | `<`, `<=`,| operators are defined for both  |
-- | `>`, `>=` | integer and boolean values.     |
-- +-----------+---------------------------------+
-- | `+`, `-`  | Addition and subtraction        |
-- +-----------+---------------------------------+
-- | `*`, `/`  | Multiplication and division     |
-- +-----------+---------------------------------+
-- | `-`       | Numerical negation              |
-- +-----------+---------------------------------+
data BinaryOp
  = -- | Binary `or`, for logical disjunction (e.g., `true or false`). `or` is left-associative.
    OpOr
  | -- | Binary `and`, for logical conjunction (e.g., `true and true`). `and` is left-associative.
    OpAnd
  | -- | Binary `==`, for equality (e.g., `1 == 1`). `==` is non-associative.
    OpEq
  | -- | Binary `!=`, for inequality (e.g., `1 != 2`). `!=` is non-associative.
    OpNeq
  | -- | Binary `<`, for strictly less than (e.g., `0 < 1`). `<` is non-associative.
    OpLess
  | -- | Binary `<=`, for less than or equal (e.g., `1 <= 1` or `0 <= 1`). `<=` is non-associative.
    OpLessEq
  | -- | Binary `>`, for strictly greater than (e.g., `1 > 0`). `>` is non-associative.
    OpGreater
  | -- | Binary `>=`, for greater than or equal (e.g., `1 >= 0` or `0 >= 0`).
    -- `>=` is non-associative.
    OpGreaterEq
  | -- | Binary `+`, for addition (e.g., `1 + 1`). `+` is left-associative.
    OpPlus
  | -- | Binary `-`, for subtraction (e.g., `1 - 1`). Binary `-` is left-associative.
    OpMinus
  | -- | Binary `*`, for multiplication (e.g., `2 * 2`). `*` is left-associative.
    OpMul
  | -- | Binary `/`, for division (e.g., `2 / 2`). `/` is left-associative.
    OpDiv
  deriving (Eq, Show)

-- | 'Expr' encodes a Roo expression.
data Expr
  = -- | 'LVal' represents an 'LValue' expression, where an lvalue is something to which a value
    -- may be assigned.
    LVal SourcePos LValue
  | -- | 'ConstBool' represents a compile-time boolean constant (i.e., `true` or `false`).
    ConstBool SourcePos Bool
  | -- | 'ConstInt' represents a compile-time integer constant (e.g., `1` or `0xFF`).
    ConstInt SourcePos Integer
  | -- | 'ConstStr' represents a constant string (e.g., `"foo"`). Strings are a sequence of
    -- characters between double quotes. The following characters must be escaped in a string:
    -- `\\n`, `\\t`, and `\\"`.
    ConstStr SourcePos String
  | -- | 'BinOpExpr' represents an expression containing a 'BinaryOp' (of the form
    -- `expr BINOP expr`, e.g., `1 + 1`).
    BinOpExpr SourcePos BinaryOp Expr Expr
  | -- | 'UnOpExpr' represents an expression containing a 'UnaryOp' (of the form `UNOP expr`, e.g.
    -- `-1`).
    UnOpExpr SourcePos UnaryOp Expr
  deriving (Eq, Show)

-- | 'LValue' represents an lvalue (something to which a value may be assigned). An lvalue consists
-- of an identifier, optionally followed by an array index expression, optionally followed by a
-- record field access.
data LValue = LValue SourcePos Ident (Maybe Expr) (Maybe Ident)
  deriving (Eq, Show)

-- | 'Stmt' represents a statement, which can be a single atomic statement or a multi-line composite
-- statement.
data Stmt
  = -- | 'SAtom' represents an atomic statement (i.e., a statement which cannot be subdivided into
    -- smaller statements).
    SAtom SourcePos AtomicStmt
  | -- | 'SComp' represents a composite statement (i.e., a statement which is made up of a number
    -- of other statements, which can themselves be atomic or composite).
    SComp SourcePos CompositeStmt
  deriving (Eq, Show)

-- | 'AtomicStmt' represents one of the five types of atomic statement.
data AtomicStmt
  = -- | 'Assign' represents an assignment statement (storing the value of an expression in an
    -- lvalue).
    Assign LValue Expr
  | -- | 'Read' represents a read statement (reading a value from stdin into an lvalue).
    Read LValue
  | -- | 'Write' represents a write statement (writing the value of an expression to stdout without a
    -- trailing newline).
    Write Expr
  | -- | 'WriteLn' represents a writeln statement (writing the value an expression to stdout with a
    -- trailing newline).
    WriteLn Expr
  | -- | 'Call' represents a procedure call statement (calling a given procedure with the values of
    -- the provided list of expressions as its arguments).
    Call Ident [Expr]
  deriving (Eq, Show)

-- | 'CompositeStmt' represents a statement itself made up of other statements.
data CompositeStmt
  = -- | 'IfBlock' represents an if-else statement. If no else clause is present in the source
    -- statement, the second list of 'Stmt' will be empty.
    IfBlock Expr [Stmt] [Stmt]
  | -- | 'WhileBlock' represents a while loop.
    WhileBlock Expr [Stmt]
  deriving (Eq, Show)

-- | 'BuiltinType' represents the two builtin Roo types, booleans and integers.
data BuiltinType
  = -- | 'TBool' represents the builtin `boolean` type.
    TBool
  | -- | 'TInt' represents the builtin `integer` type.
    TInt
  deriving (Eq, Show)

-- | 'FieldDecl' represents a record field declaration, consisting of a type and field name.
data FieldDecl = FieldDecl SourcePos BuiltinType Ident
  deriving (Eq, Show)

-- | 'RecordDef' represents a record type declaration, which contains at least one field declaration.
data RecordDef = RecordDef SourcePos [FieldDecl] Ident
  deriving (Eq, Show)

-- | 'ArrayType' represents the underlying type of an array type, which may be a builtin type or a
-- another declared type.
data ArrayType
  = -- | 'ArrBuiltinT' represents an underlying builtin type.
    ArrBuiltinT BuiltinType
  | -- | 'ArrAliasT' represents an underlying declared type.
    ArrAliasT Ident
  deriving (Eq, Show)

-- | 'ArrayDef' represents an array type definition. Arrays have a set (positive integer) size, an
-- underlying type, and a name.
data ArrayDef = ArrayDef SourcePos Integer ArrayType Ident
  deriving (Eq, Show)

-- | 'ProcParamPassMode' represents how a procedure parameter is passed: by reference, or by value.
-- This only applies to builtin types; declared types are always passed by reference.
data ProcParamPassMode
  = -- | 'PassByRef' indicates that the parameter is passed by reference.
    PassByRef
  | -- | 'PassByVal' indicates that the parameter is passed by value.
    PassByVal
  deriving (Eq, Show)

-- | 'ProcParamType' represents the type of a procedure parameter.
data ProcParamType
  = -- | 'ParamBuiltinT' represents a parameter of a builtin type and whether it is passed by
    -- reference or value.
    ParamBuiltinT BuiltinType ProcParamPassMode
  | -- | 'ParamAliasT' represents a parameter of a declared type.
    ParamAliasT Ident
  deriving (Eq, Show)

-- | 'ProcParam' represents a procedure parameter, including its type and name.
data ProcParam = ProcParam SourcePos ProcParamType Ident
  deriving (Eq, Show)

-- | 'ProcHead' represents a procedure header: the procedure name and its (possibly empty) list of
-- parameters.
data ProcHead = ProcHead SourcePos Ident [ProcParam]
  deriving (Eq, Show)

-- | 'VarType' represents the type of a variable.
data VarType
  = -- | 'VarBuiltinT' represents a variable of a builtin type.
    VarBuiltinT BuiltinType
  | -- | 'VarAliasT' represents a variable of a declared type.
    VarAliasT Ident
  deriving (Eq, Show)

-- | 'VarDecl' represents a variable declaration. A declaration can include multiple different
-- variable names, which is equivalent to declaring multiple variables of the same shared type.
data VarDecl = VarDecl SourcePos VarType [Ident]
  deriving (Eq, Show)

-- | 'ProcBody' represents the body of a procedure, including the (possibly empty) list of
-- local variable declarations, and the list of statements which are executed.
data ProcBody = ProcBody [VarDecl] [Stmt]
  deriving (Eq, Show)

-- | 'Procedure' represents a complete procedure definition.
data Procedure = Procedure ProcHead ProcBody
  deriving (Eq, Show)

-- | 'Program' represents a full Roo program: a (possibly empty) series of record definitions,
-- a (possibly empty) series of array definitions, and a series of procedure definitions.
data Program = Program [RecordDef] [ArrayDef] [Procedure]
  deriving (Eq, Show)

--
-- Source position extraction
--

-- TODO: does this return the start position or the operator position?
exprPos :: Expr -> SourcePos
exprPos (ConstStr pos _) = pos
exprPos (ConstInt pos _) = pos
exprPos (ConstBool pos _) = pos
exprPos (UnOpExpr pos _ _) = pos
exprPos (BinOpExpr pos _ _ _) = pos
exprPos (LVal pos _) = pos

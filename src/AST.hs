-- |
-- Module: AST
-- Description: Data types which encode the abstract syntax tree of a valid Roo program.
-- Maintainer: Stewart Webb <sjwebb@student.unimelb.edu.au>
--             Ben Frengley <bfrengley@student.unimelb.edu.au>
--
--  A Roo program consists of: any number of record type definitions, any number of array type
--  definitions, and at least one procedure definition.
--
--  This module is defined in a bottom-up order, beginning with the smaller components of the grammar
--  and building up to a full Roo program.
module AST where

-- | 'Ident' encodes a Roo identifier.
newtype Ident = Ident String deriving (Eq, Show)

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
  = -- | 'LVal' represents an 'LValue' expression.
    LVal LValue
  | -- | 'ConstBool' represents a compile-time boolean constant (i.e., `true` or `false`).
    ConstBool Bool
  | -- | 'ConstInt' represents a compile-time integer constant (e.g., `1` or `0xFF`).
    ConstInt Integer
  | -- | 'ConstStr' represents a constant string (e.g., `"foo"`). Strings are a sequence of
    -- characters between double quotes. The following characters must be escaped in a string:
    -- `\\n`, `\\t`, and `\\"`.
    ConstStr String
  | -- | 'BinOpExpr' represents an expression containing a 'BinaryOp' (of the form
    -- `expr BINOP expr`, e.g., `1 + 1`).
    BinOpExpr BinaryOp Expr Expr
  | -- | 'UnOpExpr' represents an expression containing a 'UnaryOp' (of the form `UNOP expr`, e.g.
    -- `-1`).
    UnOpExpr UnaryOp Expr
  deriving (Eq, Show)

-- | 'LValue' represents an lvalue (something to which a value may be assigned). An lvalue consists
-- of an identifier, optionally followed by an index expression, optionally followed by a record
-- field access.
data LValue = LValue Ident (Maybe Expr) (Maybe Ident)
  deriving (Eq, Show)

data Stmt = SAtom AtomicStmt | SComp CompositeStmt
  deriving (Eq, Show)

data AtomicStmt
  = Assign LValue Expr
  | Read LValue
  | Write Expr
  | WriteLn Expr
  | Call Ident [Expr]
  deriving (Eq, Show)

data CompositeStmt
  = IfBlock Expr [Stmt] [Stmt]
  | WhileBlock Expr [Stmt]
  deriving (Eq, Show)

data BuiltinType = TBool | TInt deriving (Eq, Show)

data FieldDecl = FieldDecl BuiltinType Ident
  deriving (Eq, Show)

data RecordDef = RecordDef [FieldDecl] Ident
  deriving (Eq, Show)

data ArrayType
  = ArrBuiltinT BuiltinType
  | ArrAliasT Ident
  deriving (Eq, Show)

data ArrayDef = ArrayDef Integer ArrayType Ident
  deriving (Eq, Show)

data ProcParamPassMode
  = PassByRef
  | PassByVal
  deriving (Eq, Show)

data ProcParamType
  = ParamBuiltinT BuiltinType ProcParamPassMode
  | ParamAliasT Ident
  deriving (Eq, Show)

data ProcParam = ProcParam ProcParamType Ident
  deriving (Eq, Show)

data ProcHead = ProcHead Ident [ProcParam]
  deriving (Eq, Show)

-- this is the same type as ArrayType
data VarType = VarBuiltinT BuiltinType | VarAliasT Ident
  deriving (Eq, Show)

data VarDecl = VarDecl VarType [Ident]
  deriving (Eq, Show)

data ProcBody = ProcBody [VarDecl] [Stmt]
  deriving (Eq, Show)

data Procedure = Procedure ProcHead ProcBody
  deriving (Eq, Show)

data Program = Program [RecordDef] [ArrayDef] [Procedure]
  deriving (Eq, Show)

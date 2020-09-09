module AST where

type Ident = String

data UnaryOp
  = OpNeg
  | OpNot
  deriving (Eq, Show)

data BinaryOp
  = OpOr
  | OpAnd
  | OpEq
  | OpNeq
  | OpLess
  | OpLessEq
  | OpGreater
  | OpGreaterEq
  | OpPlus
  | OpMinus
  | OpMul
  | OpDiv
  deriving (Eq, Show)

data Expr
  = LVal LValue
  | ConstBool Bool
  | ConstInt Int
  | ConstStr String
  | BinOpExpr BinaryOp Expr Expr
  | UnOpExpr UnaryOp Expr
  deriving (Eq, Show)

-- An l-value consists of an identifier, an option index expression, and an optional field
data LValue = LValue Ident (Maybe Expr) (Maybe Ident)
  deriving (Eq, Show)

data Stmt
  = Assign LValue Expr
  | Read LValue
  | Write Expr
  | WriteLn Expr
  | Call Ident [Expr]
  | IfBlock Expr [Stmt] [Stmt]
  | WhileBlock Expr [Stmt]
  deriving (Eq, Show)

data BuiltinType = TBool | TInt deriving (Eq, Show)

data FieldDecl = FieldDecl BuiltinType Ident
  deriving (Eq, Show)

data RecordDef = RecordDef Ident [FieldDecl]
  deriving (Eq, Show)

data ArrayType
  = ArrBuiltinT BuiltinType
  | ArrAliasT Ident
  deriving (Eq, Show)

data ArrayDef = ArrayDef Ident ArrayType Int
  deriving (Eq, Show)

data ProcParamPassType -- better name?
  = PassByRef
  | PassByVal
  deriving (Eq, Show)

data ProcParamType
  = ParamBuiltinT BuiltinType ProcParamPassType
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

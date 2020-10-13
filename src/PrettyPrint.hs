{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: PrettyPrint
-- Description: A pretty printer for Roo programs.
-- Maintainer: Stewart Webb <sjwebb@student.unimelb.edu.au>
--             Ben Frengley <bfrengley@student.unimelb.edu.au>
--
-- This module defines pretty printers for the various parts of a Roo program. It is designed in
-- such a way that printers are entirely indentation agnostic, i.e., no printer needs to know
-- the indentation level of its context to result in valid indentation. This is achieved by
-- dealing in lines until the very end, rather than joining everything into a single string as it
-- goes. This allows each printer to indent its children as necessary without awareness of its own
-- parent.
--
-- It's possible that this approach has performance implications, but we have left them unexplored
-- as it is adequate for this project.
--
-- This module is defined in a bottom-up order, beginning with the smaller components of the grammar
-- and building up to a full Roo program.
module PrettyPrint where

import AST
import Data.List (intercalate)
import qualified Data.Text as T

-- | 'prettyPrint' converts a Roo program into a consistently formatted string.
prettyPrint :: Program -> T.Text
prettyPrint (Program recordDefs arrayDefs procDefs) =
  let records = concatMap pPrintRecord recordDefs
      arrays = map pPrintArrayDef arrayDefs
      -- insert a blank line between each procedure
      procs = intercalate [""] $ map pPrintProcedure procDefs
      defs = records <> arrays
   in T.unlines $ case defs of
        [] -> procs
        _ -> defs <> [""] <> procs

--
-- Expressions
--

-- | 'Precedence' indicates the precedence of an operator in an expression. We use this to
-- determine when a sub-expression in a 'Expr' AST node requires wrapping in parentheses
-- for the semantics of the expression to be maintained in printed form (i.e., parenthesis
-- optimisation). In general, parentheses are only required when a child 'Expr' has an operator of
-- lower precedence than its parent, as operators of higher precedence are parsed first and thus
-- fall lowest in the 'Expr' tree.
--
-- We assign each operator a numerical precedence corresponding to their position in the operator
-- precedence list, where lower precedence is a lower (non-negative) number. We increment by 3
-- each level into the list, as this gives us some room to handle special cases.
type Precedence = Integer

-- | 'basePrecedence' is the precedence of an empty expression. It's lower than all other
-- precedences to ensure that it never forces parentheses.
basePrecedence :: Precedence
basePrecedence = 0

-- | 'binOpPrecedence' returns the precedence of a binary operator, corresponding to its position
-- in the precedence list. Operators which occur alongside each other in the list return an equal
-- value.
binOpPrecedence :: BinaryOp -> Precedence
binOpPrecedence OpOr = 3
binOpPrecedence OpAnd = 6
binOpPrecedence OpEq = 12
binOpPrecedence OpNeq = 12
binOpPrecedence OpLess = 12
binOpPrecedence OpLessEq = 12
binOpPrecedence OpGreater = 12
binOpPrecedence OpGreaterEq = 12
binOpPrecedence OpPlus = 15
binOpPrecedence OpMinus = 15
binOpPrecedence OpMul = 18
binOpPrecedence OpDiv = 18

-- | 'isNonAssoc' returns true if a precedence corresponds to a non-associative operator.
isNonAssoc :: Precedence -> Bool
isNonAssoc prec = prec == 12

-- Non-associative operators can only be chained if parentheses are present, as opposed to
-- associative operators. In order to force parentheses to be retained, we adjust their precedence
-- one partial increment upwards to make them higher precedence than their equal precedence
-- children in an expression tree.
adjustPrecedence :: Precedence -> Precedence
adjustPrecedence prec
  | isNonAssoc prec = prec + 1
  | otherwise = prec

-- the operators * and / have the same precedence, but / isn't associative
-- these special sentinel values allow us to detect when a mul/div expression occurs on the RHS of
-- a * or / so that we can keep the parentheses in place

-- | 'rhsMulPrecedence' is the modified precedence of the multiplication operator (*) used when
-- determining when to parenthesise its right child. It is one partial increment higher than the
-- normal * precedence.
rhsMulPrecedence :: Precedence
rhsMulPrecedence = 19

-- | 'rhsDivPrecedence' is the modified precedence of the division operator (/) used when determining
-- when to parenthesise its right child. It is two partial increments higher than the normal /
-- precedence.
rhsDivPrecedence :: Precedence
rhsDivPrecedence = 20

-- | 'rhsMinusPrecedence' is the modified precedence of the subtraction operator (-) used when
-- determinig when to parenthesise its right child. It is one partial increment higher than the
-- normal - precedence.
rhsMinusPrecedence :: Precedence
rhsMinusPrecedence = 16

-- | 'unOpPrecedence' returns the precedence of a unary operator, corresponding to its position in
-- the precedence list.
unOpPrecedence :: UnaryOp -> Precedence
unOpPrecedence OpNot = 9
unOpPrecedence OpNeg = 21

-- | 'pPrintBinOp' prints a binary operator with appropriate spaces.
pPrintBinOp :: BinaryOp -> T.Text
pPrintBinOp OpOr = " or "
pPrintBinOp OpAnd = " and "
pPrintBinOp OpEq = " = "
pPrintBinOp OpNeq = " != "
pPrintBinOp OpLess = " < "
pPrintBinOp OpLessEq = " <= "
pPrintBinOp OpGreater = " > "
pPrintBinOp OpGreaterEq = " >= "
pPrintBinOp OpPlus = " + "
pPrintBinOp OpMinus = " - "
pPrintBinOp OpMul = " * "
pPrintBinOp OpDiv = " / "

-- | 'pPrintUnOp' prints a unary operator with appropriate spaces.
pPrintUnOp :: UnaryOp -> T.Text
pPrintUnOp OpNot = "not "
pPrintUnOp OpNeg = "-"

-- | 'pPrintExpr' pretty prints an expression with optimised parentheses and appropriate spacing.
pPrintExpr :: Expr -> T.Text
pPrintExpr = pPrintExpr' basePrecedence

-- 'pPrintExpr'' does the heavy lifting for 'pPrintExpr'. It recursively descends through an
-- 'Expr' AST node, tracking the precedence of the parent operator and pretty printing each node
-- appropriately based on type and the parent's precedence.
pPrintExpr' :: Precedence -> Expr -> T.Text
pPrintExpr' _ (LVal _ lv) = pPrintLval lv
pPrintExpr' _ (ConstBool _ b) = T.toLower $ showT b
pPrintExpr' _ (ConstInt _ n) = showT n
pPrintExpr' _ (ConstStr _ s) = "\"" <> T.pack s <> "\""
pPrintExpr' prec (UnOpExpr _ op expr) =
  let opPrec = unOpPrecedence op
      subExprStr = pPrintExpr' opPrec expr
      exprStr = pPrintUnOp op <> subExprStr
   in if prec > opPrec then "(" <> exprStr <> ")" else exprStr
pPrintExpr' prec (BinOpExpr _ op lhsExpr rhsExpr) =
  -- parenthesise determines when to add parentheses to an expression based on the precedence of
  -- the parent operator (the operator of the 'Expr' node of which the current node is the child).
  let parenthesise :: Precedence -> Precedence -> T.Text -> T.Text
      parenthesise parentPrec exprPrec expr
        -- 1*(2*3) is the special case of our special case: * is associative, so we can ignore
        -- the fact that it produces a different parse tree and just remove the parentheses
        | parentPrec == rhsMulPrecedence && op == OpMul = expr
        -- this expression has lower precedence than the parent expression yet comes lower in the
        -- parse tree, which means it must have been parenthesised
        | parentPrec > exprPrec = "(" <> expr <> ")"
        | otherwise = expr

      opPrec = binOpPrecedence op
      pPrintChild = pPrintExpr' (adjustPrecedence opPrec)

      left = pPrintChild lhsExpr
      right = case op of
        -- if the RHS of a mul/div expression is a div expression, it was parenthesised
        -- we pretend the operator has a higher precedence to get the right parentheses
        -- if the RHS isn't a div expression, the special precedences are positioned the same
        -- relative to all other operators, so it doesn't matter
        OpDiv -> pPrintExpr' rhsDivPrecedence rhsExpr
        OpMul -> pPrintExpr' rhsMulPrecedence rhsExpr
        -- minus is similar: 1 - 1 + 1 != 1 - (1 + 1)
        OpMinus -> pPrintExpr' rhsMinusPrecedence rhsExpr
        _ -> pPrintChild rhsExpr
   in parenthesise prec opPrec $ left <> pPrintBinOp op <> right

-- | 'pPrintLval' prints an lvalue with spaces between each of the parts removed, and the possible
-- index expression formatted appropriately.
pPrintLval :: LValue -> T.Text
pPrintLval (LValue _ ident idx field) =
  let showIndex i = "[" <> pPrintExpr i <> "]"
      showField f = "." <> pPrintIdent f
   in pPrintIdent ident <> maybe "" showIndex idx <> maybe "" showField field

--
-- Statements
--

-- | 'pPrintStmt' prints a composite or atomic statement. It returns a list of the lines making up
-- the statement. Atomic statements are always a list of a single item, while composite statements
-- vary.
pPrintStmt :: Stmt -> [T.Text]
pPrintStmt (SAtom _ atom) = [pPrintAtomicStmt atom <> semi]
pPrintStmt (SComp _ comp) = pPrintCompositeStmt comp

-- | 'pPrintAtomicStmt' prints an atomic statement with appropriate formatting depending on the
-- type of the statement in question. It does not include the trailing semicolon.
pPrintAtomicStmt :: AtomicStmt -> T.Text
pPrintAtomicStmt (Assign lval expr) = pPrintLval lval <> " <- " <> pPrintExpr expr
pPrintAtomicStmt (Read lval) = "read " <> pPrintLval lval
pPrintAtomicStmt (Write expr) = "write " <> pPrintExpr expr
pPrintAtomicStmt (WriteLn expr) = "writeln " <> pPrintExpr expr
pPrintAtomicStmt (Call ident exprs) =
  let exprList = commaSep pPrintExpr exprs
   in "call " <> pPrintIdent ident <> "(" <> exprList <> ")"

-- | 'pPrintCompositeStmt' prints each of the lines making up a composite statement into a list.
-- It indents the child statements which make up the composite statement by a single level. If a
-- child is another composite statement, it will already have indented its children by a level,
-- so the indentation for all child statements will be correct regardless of depth, despite
-- 'pPrintCompositeStmt' being unaware of the context of the statement.
pPrintCompositeStmt :: CompositeStmt -> [T.Text]
pPrintCompositeStmt (IfBlock expr mainStmts elseStmts) =
  let prettyMain = indentMany pPrintStmt mainStmts
      prettyElse = indentMany pPrintStmt elseStmts
      -- only include an "else" if there are statements in the else block
      elseBlock = case prettyElse of
        [] -> []
        es -> "else" : es
   in concat
        [ ["if " <> pPrintExpr expr <> " then"],
          prettyMain,
          elseBlock,
          ["fi"]
        ]
pPrintCompositeStmt (WhileBlock expr body) =
  let prettyBody = map indent $ concatMap pPrintStmt body
   in concat
        [ ["while " <> pPrintExpr expr <> " do"],
          prettyBody,
          ["od"]
        ]

--
-- Records
--

-- | 'pPrintRecord' prints a record type definition.
pPrintRecord :: RecordDef -> [T.Text]
pPrintRecord (RecordDef _ (field : fields) name) =
  let -- pPrintField prints a field declaration with the specified line opener
      pPrintField open (FieldDecl _ t ident) = T.unwords [open, pPrintBuiltinType t, pPrintIdent ident]
      -- start the first line with a brace
      prettyOpen = indent $ pPrintField "{" field
      -- every other line starts with a semicolon
      prettyFields = indentMap (pPrintField ";") fields
   in concat
        [ ["record"],
          [prettyOpen],
          prettyFields,
          [indent "} " <> pPrintIdent name <> semi]
        ]

--
-- Array definitions
--

-- | 'pPrintArrayDef' prints an array type definition.
pPrintArrayDef :: ArrayDef -> T.Text
pPrintArrayDef (ArrayDef _ size t name) =
  T.unwords ["array[" <> showT size <> "]", pPrintArrayType t, pPrintIdent name] <> semi
  where
    pPrintArrayType t = case t of
      (ArrBuiltinT b) -> pPrintBuiltinType b
      (ArrAliasT i) -> pPrintIdent i

--
-- Procedures
--

-- | 'pPrintProcedure' prints a complete procedure as its component lines.
pPrintProcedure :: Procedure -> [T.Text]
pPrintProcedure (Procedure head body) = pPrintProcHead head : pPrintProcBody body

-- | 'pPrintProcHead' prints the procedure header as a single line.
pPrintProcHead :: ProcHead -> T.Text
pPrintProcHead (ProcHead _ name params) =
  let pPrintParam (ProcParam _ t ident) = T.unwords [pPrintParamType t, pPrintIdent ident]
      paramList = commaSep pPrintParam params
   in T.unwords ["procedure", pPrintIdent name, "(" <> paramList <> ")"]

-- | 'pPrintParamType' prints a procedure parameter type.
pPrintParamType :: ProcParamType -> T.Text
pPrintParamType (ParamBuiltinT t PassByRef) = pPrintBuiltinType t
pPrintParamType (ParamBuiltinT t PassByVal) = pPrintBuiltinType t <> " val"
pPrintParamType (ParamAliasT t) = pPrintIdent t

-- | 'pPrintProcBody' prints a procedure body, including the variable declarations and a
-- brace-delimited series of statements.
pPrintProcBody :: ProcBody -> [T.Text]
pPrintProcBody (ProcBody vars body) =
  concat
    [ indentMap pPrintVarDecl vars,
      ["{"],
      indentMany pPrintStmt body,
      ["}"]
    ]

-- | 'pPrintVarDecl' prints a local variable declaration, preserving the original groups of
-- declarations (i.e., all variables which were declared in the same declaration in the source
-- will be declared in the same declaration in the pretty printed version).
pPrintVarDecl :: VarDecl -> T.Text
pPrintVarDecl (VarDecl _ t names) =
  pPrintVarType t <> " " <> commaSep pPrintIdent names <> semi

-- | 'pPrintVarType' prints a variable type name.
pPrintVarType :: VarType -> T.Text
pPrintVarType (VarBuiltinT t) = pPrintBuiltinType t
pPrintVarType (VarAliasT t) = pPrintIdent t

--
-- Utility functions
--

-- | 'pPrintBuiltin' prints a builtin type name.
pPrintBuiltinType :: BuiltinType -> T.Text
pPrintBuiltinType TBool = "boolean"
pPrintBuiltinType TInt = "integer"

-- | 'pPrintIdent' converts an identifier to 'T.Text'.
pPrintIdent :: Ident -> T.Text
pPrintIdent (Ident _ i) = T.pack i

-- | 'indent' applies a single level of indentation (i.e., four leading spaces) to the given line.
indent :: T.Text -> T.Text
indent s = "    " <> s

-- | 'indentMap' maps a printing function which produces a single line across a series of inputs
-- and indents each resulting line.
indentMap :: (a -> T.Text) -> [a] -> [T.Text]
indentMap f = map (indent . f)

-- | 'indentMany' maps a printing function which produces multiple lines across a series of inputs
-- and indents each resulting line, concatenating them into a single list of lines.
indentMany :: (a -> [T.Text]) -> [a] -> [T.Text]
indentMany f = map indent . concatMap f

semi :: T.Text
semi = ";"

-- | 'commaSep' maps a printing function across a list of inputs and joins the inputs with a comma
-- (and a space).
commaSep :: (a -> T.Text) -> [a] -> T.Text
commaSep f = T.intercalate ", " . map f

-- | 'showT' converts a 'Show' type to 'T.Text'.
showT :: Show a => a -> T.Text
showT = T.pack . show

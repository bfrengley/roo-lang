-- Stewart Webb - sjwebb@student.unimelb.edu.au
-- Ben Frengley - bfrengley@student.unimelb.edu.au
{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import AST
import Data.List (intercalate)
import Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Data.Text.IO as I

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

type Precedence = Int

basePrecedence :: Precedence
basePrecedence = 0

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

isNonAssoc :: Precedence -> Bool
isNonAssoc prec = prec == 12

-- Harald why
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
rhsMulPrecedence :: Precedence
rhsMulPrecedence = 19

rhsDivPrecedence :: Precedence
rhsDivPrecedence = 20

rhsMinusPrecedence :: Precedence
rhsMinusPrecedence = 16

unOpPrecedence :: UnaryOp -> Precedence
unOpPrecedence OpNot = 9
unOpPrecedence OpNeg = 21

pPrintLval :: LValue -> T.Text
pPrintLval (LValue ident idx field) =
  let showIndex i = "[" <> pPrintExpr i <> "]"
      showField f = "." <> pPrintIdent f
   in pPrintIdent ident <> maybe "" showIndex idx <> maybe "" showField field

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

pPrintUnOp :: UnaryOp -> T.Text
pPrintUnOp OpNot = "not "
pPrintUnOp OpNeg = "-"

pPrintExpr :: Expr -> T.Text
pPrintExpr = pPrintExpr' basePrecedence

pPrintExpr' :: Precedence -> Expr -> T.Text
pPrintExpr' _ (LVal lv) = pPrintLval lv
pPrintExpr' _ (ConstBool b) = T.toLower $ showT b
pPrintExpr' _ (ConstInt n) = showT n
pPrintExpr' _ (ConstStr s) = "\"" <> T.pack s <> "\""
pPrintExpr' prec (UnOpExpr op expr) =
  let exprStr = pPrintExpr' (unOpPrecedence op) expr
   in pPrintUnOp op <> exprStr
pPrintExpr' prec (BinOpExpr op lhsExpr rhsExpr) =
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

--
-- Statements
--

pPrintStmt :: Stmt -> [T.Text]
pPrintStmt (SAtom atom) = [semi $ pPrintAtomicStmt atom]
pPrintStmt (SComp comp) = pPrintCompositeStmt comp

pPrintAtomicStmt :: AtomicStmt -> T.Text
pPrintAtomicStmt (Assign lval expr) = pPrintLval lval <> " <- " <> pPrintExpr expr
pPrintAtomicStmt (Read lval) = "read " <> pPrintLval lval
pPrintAtomicStmt (Write expr) = "write " <> pPrintExpr expr
pPrintAtomicStmt (WriteLn expr) = "writeln " <> pPrintExpr expr
pPrintAtomicStmt (Call ident exprs) =
  let exprList = commaSep pPrintExpr exprs
   in "call " <> pPrintIdent ident <> "(" <> exprList <> ")"

pPrintCompositeStmt :: CompositeStmt -> [T.Text]
pPrintCompositeStmt (IfBlock expr mainStmts elseStmts) =
  let prettyMain = indentMany pPrintStmt mainStmts
      prettyElse = indentMany pPrintStmt elseStmts
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

pPrintRecord :: RecordDef -> [T.Text]
pPrintRecord (RecordDef (field : fields) name) =
  let pPrintField open (FieldDecl t ident) = T.unwords [open, pPrintBuiltinType t, pPrintIdent ident]
      prettyOpen = indent $ pPrintField "{" field
      prettyFields = indentMap (pPrintField ";") fields
   in concat
        [ ["record"],
          [prettyOpen],
          prettyFields,
          [semi $ indent "} " <> pPrintIdent name]
        ]

--
-- Array definitions
--

pPrintArrayDef :: ArrayDef -> T.Text
pPrintArrayDef (ArrayDef size t name) =
  semi $ T.unwords ["array[" <> showT size <> "]", pPrintArrayType t, pPrintIdent name]
  where
    pPrintArrayType t = case t of
      (ArrBuiltinT b) -> pPrintBuiltinType b
      (ArrAliasT i) -> pPrintIdent i

--
-- Procedures
--

pPrintProcedure :: Procedure -> [T.Text]
pPrintProcedure (Procedure head body) = pPrintProcHead head : pPrintProcBody body

pPrintProcHead :: ProcHead -> T.Text
pPrintProcHead (ProcHead name params) =
  let pPrintParam (ProcParam t ident) = T.unwords [pPrintParamType t, pPrintIdent ident]
      paramList = commaSep pPrintParam params
   in T.unwords ["procedure", pPrintIdent name, "(" <> paramList <> ")"]

pPrintParamType :: ProcParamType -> T.Text
pPrintParamType (ParamBuiltinT t PassByRef) = pPrintBuiltinType t
pPrintParamType (ParamBuiltinT t PassByVal) = pPrintBuiltinType t <> " val"
pPrintParamType (ParamAliasT t) = pPrintIdent t

pPrintProcBody :: ProcBody -> [T.Text]
pPrintProcBody (ProcBody vars body) =
  concat
    [ indentMap pPrintVarDecl vars,
      ["{"],
      indentMany pPrintStmt body,
      ["}"]
    ]

pPrintVarDecl :: VarDecl -> T.Text
pPrintVarDecl (VarDecl t names) =
  semi $ pPrintVarType t <> " " <> commaSep pPrintIdent names

pPrintVarType :: VarType -> T.Text
pPrintVarType (VarBuiltinT t) = pPrintBuiltinType t
pPrintVarType (VarAliasT t) = pPrintIdent t

--
-- Utility functions
--

pPrintBuiltinType :: BuiltinType -> T.Text
pPrintBuiltinType TBool = "boolean"
pPrintBuiltinType TInt = "integer"

pPrintIdent :: Ident -> T.Text
pPrintIdent (Ident i) = T.pack i

indent :: T.Text -> T.Text
indent s = "    " <> s

indentMany :: (a -> [T.Text]) -> [a] -> [T.Text]
indentMany f = map indent . concatMap f

indentMap :: (a -> T.Text) -> [a] -> [T.Text]
indentMap f = map (indent . f)

semi :: T.Text -> T.Text
semi s = s <> ";"

commaSep :: (a -> T.Text) -> [a] -> T.Text
commaSep f = T.intercalate ", " . map f

showT :: Show a => a -> T.Text
showT = T.pack . show

tryPrint :: Show e => (a -> [T.Text]) -> Either e a -> IO ()
tryPrint f (Left err) = print err
tryPrint f (Right r) = I.putStrLn $ T.unlines $ f r

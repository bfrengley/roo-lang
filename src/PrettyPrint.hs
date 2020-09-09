{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import AST
import Data.List (intercalate)
import Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Data.Text.IO as I

-- prettyPrint :: Program -> T.Text
-- prettyPrint rootNode =
--     case rootNode of
--         Procedure -> "[Procedure]"

-- prettyPrint :: Program -> IO ()
-- prettyPrint (Program records arrs procs) =
--   do
--     map printRecord records

-- printRecord :: Record -> IO ()
-- printRecord record =
--   firstRec:nextRecs
--   where firstRec:nextRecs = records

prettyPrint :: Program -> T.Text
prettyPrint (Program recordDefs arrayDefs procDefs) =
  let records = map pPrintRecord recordDefs
      arrays = map pPrintArrayDef arrayDefs
      procs = concatMap pPrintProcedure procDefs
      defs = concat records <> arrays
   in case defs of
        [] -> T.unlines procs
        _ -> T.unlines $ defs <> [""] <> procs

-- case rootNode of
--     Procedure -> "[Procedure]"

--
-- Expressions
--

type Precedence = Int

basePrecedence :: Precedence
basePrecedence = 0

binOpPrecedence :: BinaryOp -> Precedence
binOpPrecedence OpOr = 1
binOpPrecedence OpAnd = 2
binOpPrecedence OpEq = 4
binOpPrecedence OpNeq = 4
binOpPrecedence OpLess = 4
binOpPrecedence OpLessEq = 4
binOpPrecedence OpGreater = 4
binOpPrecedence OpGreaterEq = 4
binOpPrecedence OpPlus = 5
binOpPrecedence OpMinus = 5
binOpPrecedence OpMul = 6
binOpPrecedence OpDiv = 6

-- the operators * and / have the same precedence, but / isn't associative
-- these special sentinel values allow us to detect when a mul/div expression occurs on the RHS of
-- a * or / so that we can keep the parentheses in place
rhsMulPrecedence :: Precedence
rhsMulPrecedence = 7

rhsDivPrecedence :: Precedence
rhsDivPrecedence = 8

unOpPrecedence :: UnaryOp -> Precedence
unOpPrecedence OpNot = 3
-- this is higher than expected to allow us to do special handling of mul/div associativity
unOpPrecedence OpNeg = 9

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
  let wrap :: Precedence -> Precedence -> T.Text -> T.Text
      wrap parentPrec exprPrec expr
        -- 1*(2*3) is the special case of our special case: * is associative, so we can ignore
        -- the fact that it produces a different parse tree and just remove the parentheses
        | parentPrec == rhsMulPrecedence && op == OpMul = expr
        -- this expression has lower precedence than the parent expression yet comes lower in the
        -- parse tree, which means it must have been parenthesised
        | parentPrec > exprPrec = "(" <> expr <> ")"
        | otherwise = expr

      opPrec = binOpPrecedence op
      left = pPrintExpr' opPrec lhsExpr
      right = case op of
        -- if the RHS of a mul/div expression is a div expression, it was parenthesised
        -- we pretend the operator has a higher precedence to get the right parentheses
        -- if the RHS isn't a div expression, the special precedences are positioned the same
        -- relative to all other operators, so it doesn't matter
        OpDiv -> pPrintExpr' rhsDivPrecedence rhsExpr
        OpMul -> pPrintExpr' rhsMulPrecedence rhsExpr
        _ -> pPrintExpr' opPrec rhsExpr
   in wrap prec opPrec $ left <> pPrintBinOp op <> right

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
  let exprList = T.intercalate ", " $ map pPrintExpr exprs
   in "call " <> pPrintIdent ident <> "(" <> exprList <> ")"

pPrintCompositeStmt :: CompositeStmt -> [T.Text]
pPrintCompositeStmt (IfBlock expr mainStmts elseStmts) =
  let prettyMain = indentMany pPrintStmt mainStmts
      prettyElse = indentMany pPrintStmt elseStmts
      elseBlock = case prettyElse of
        [] -> []
        es -> "else" : es
   in concat [["if " <> pPrintExpr expr <> " then"], prettyMain, elseBlock, ["fi"]]
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
pPrintRecord (RecordDef name (field : fields)) =
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
pPrintArrayDef (ArrayDef name t size) =
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
  T.unwords ["procedure", pPrintIdent name, "(" <> paramList <> ")"]
  where
    pPrintParam (ProcParam t ident) = T.unwords [pPrintParamType t, pPrintIdent ident]
    paramList = T.intercalate ", " (map pPrintParam params)

pPrintParamType :: ProcParamType -> T.Text
pPrintParamType (ParamBuiltinT t PassByRef) = pPrintBuiltinType t
pPrintParamType (ParamBuiltinT t PassByVal) = pPrintBuiltinType t <> " val"
pPrintParamType (ParamAliasT t) = pPrintIdent t

pPrintProcBody :: ProcBody -> [T.Text]
pPrintProcBody (ProcBody vars body) =
  concat [indentMap pPrintVarDecl vars, ["{"], indentMany pPrintStmt body, ["}"]]

pPrintVarDecl :: VarDecl -> T.Text
pPrintVarDecl (VarDecl t names) =
  semi $ pPrintVarType t <> " " <> T.intercalate ", " (map pPrintIdent names)

pPrintVarType :: VarType -> T.Text
pPrintVarType (VarBuiltinT t) = pPrintBuiltinType t
pPrintVarType (VarAliasT t) = pPrintIdent t

--
-- Utility functions
--

pPrintBuiltinType :: BuiltinType -> T.Text
pPrintBuiltinType TBool = "boolean"
pPrintBuiltinType TInt = "integer"

pPrintIdent :: String -> T.Text
pPrintIdent = T.pack

indent :: T.Text -> T.Text
indent s = "    " <> s

indentMany :: (a -> [T.Text]) -> [a] -> [T.Text]
indentMany f = map indent . concatMap f

indentMap :: (a -> T.Text) -> [a] -> [T.Text]
indentMap f = map (indent . f)

semi :: T.Text -> T.Text
semi s = s <> ";"

showT :: Show a => a -> T.Text
showT = T.pack . show

tryPrint :: Show e => (a -> [T.Text]) -> Either e a -> IO ()
tryPrint f (Left err) = print err
tryPrint f (Right r) = I.putStrLn $ T.unlines $ f r
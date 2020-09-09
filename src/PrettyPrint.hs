module PrettyPrint where

import AST
import Control.Monad.State
import Data.List (intercalate)
import Data.Maybe (maybe)

-- prettyPrint :: Program -> String
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

prettyPrint :: Program -> String
prettyPrint (Program recordDefs arrayDefs mainProc) =
  unlines
    [ unlines recLines,
      unlines arrayLines,
      unlines mainProcLines
    ]
  where
    recLines = ["sample", "    rec", "    lines"]
    arrayLines = ["sample", "    array", "    lines"]
    mainProcLines = ["sample", "    main", "    lines"]

-- case rootNode of
--     Procedure -> "[Procedure]"

--
-- Expression pretty printer
--

type Precedence = Int

type IndentationState = State Int String

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

pPrintLval :: LValue -> String
pPrintLval (LValue ident idx field) =
  let showIndex i = "[" ++ pPrintExpr i ++ "]"
      showField f = "." ++ f
   in ident ++ maybe "" showIndex idx ++ maybe "" showField field

pPrintBinOp :: BinaryOp -> String
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

pPrintUnOp :: UnaryOp -> String
pPrintUnOp OpNot = "not "
pPrintUnOp OpNeg = "-"

pPrintExpr :: Expr -> String
pPrintExpr = pPrintExpr' basePrecedence

pPrintExpr' :: Precedence -> Expr -> String
pPrintExpr' _ (LVal lv) = pPrintLval lv
pPrintExpr' _ (ConstBool b) = show b
pPrintExpr' _ (ConstInt n) = show n
pPrintExpr' _ (ConstStr s) = show s
pPrintExpr' prec (UnOpExpr op expr) =
  let exprStr = pPrintExpr' (unOpPrecedence op) expr
   in pPrintUnOp op ++ exprStr
pPrintExpr' prec (BinOpExpr op lhsExpr rhsExpr) =
  let wrap parentPrec exprPrec expr
        -- 1*(2*3) is the special case of our special case: * is associative, so we can ignore
        -- the fact that it produces a different parse tree and just remove the parentheses
        | parentPrec == rhsMulPrecedence && op == OpMul = expr
        -- this expression has lower precedence than the parent expression yet comes lower in the
        -- parse tree, which means it must have been parenthesised
        | parentPrec > exprPrec = "(" ++ expr ++ ")"
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
   in wrap prec opPrec $ left ++ pPrintBinOp op ++ right

--
-- Statements
--

pPrintStmt :: Stmt -> IndentationState
pPrintStmt (SAtom atom) = do
  depth <- get
  return $ indent depth $ pPrintAtomicStmt atom ++ ";"
pPrintStmt (SComp comp) = pPrintCompositeStmt comp

pPrintAtomicStmt :: AtomicStmt -> String
pPrintAtomicStmt (Assign lval expr) = pPrintLval lval ++ " <- " ++ pPrintExpr expr
pPrintAtomicStmt (Read lval) = "read " ++ pPrintLval lval
pPrintAtomicStmt (Write expr) = "write " ++ pPrintExpr expr
pPrintAtomicStmt (WriteLn expr) = "writeln " ++ pPrintExpr expr
pPrintAtomicStmt (Call ident exprs) =
  let exprList = intercalate ", " $ map pPrintExpr exprs
   in "call " ++ ident ++ "(" ++ exprList ++ ")"

pPrintCompositeStmt :: CompositeStmt -> IndentationState
pPrintCompositeStmt (IfBlock expr mainStmts elseStmts) = do
  depth <- get
  put 1
  prettyMain <- mapM pPrintStmt mainStmts
  prettyElse <- mapM pPrintStmt elseStmts
  put depth
  let elseBlock =
        ( case prettyElse of
            [] -> []
            es -> "else" : es
        )
  return $
    unlines $
      map (indent depth) $
        concat
          [ ["if " ++ pPrintExpr expr ++ " then"],
            prettyMain,
            elseBlock,
            ["fi"]
          ]

-- pPrintCompositeStmt (WhileBlock expr body) = do
--     indent <- get

indent :: Int -> String -> String
indent n s = replicate (4 * n) ' ' ++ s
